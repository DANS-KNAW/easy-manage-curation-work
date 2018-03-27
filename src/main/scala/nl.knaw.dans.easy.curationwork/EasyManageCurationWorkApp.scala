/**
 * Copyright (C) 2017 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy.curationwork

import java.nio.file.{Files, Path, Paths}

import sys.process._
import org.apache.commons.configuration.PropertiesConfiguration
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed
import org.apache.commons.csv.CSVFormat
import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Try
import scala.xml.XML


class EasyManageCurationWorkApp(configuration: Configuration) extends DebugEnhancedLogging {

  private val commonCurationDir = Paths.get(configuration.properties.getString("curation.common.directory"))
  private val managerCurationDirString = configuration.properties.getString("curation.personal.directory")

  private def getCurationDirectory(datamanager: Option[DatamanagerId]): Path = {
    if (datamanager.isEmpty) commonCurationDir
    else getManagerCurationDir(datamanager)
  }

  private def getManagerCurationDir(datamanager: Option[DatamanagerId]): Path = {
    Paths.get(managerCurationDirString.replace("$unix-user", datamanager.getOrElse("")))
  }

  private def getCurrentUnixUser: Option[DatamanagerId] = {
    Option("whoami" !!)
  }

  private def directoryExists(dir: Path, uuid: Option[BagId] = None): Boolean = {
    Seq(dir.resolve(uuid.getOrElse(""))).exists(Files.exists(_))
  }

  private def isSubmitted(depositProperties: PropertiesConfiguration): Boolean = {
    depositProperties.getString("state.label") == "SUBMITTED"
  }

  private def isCurated(depositProperties: PropertiesConfiguration): Boolean = {
    depositProperties.getString("curation.performed") == "yes"
  }

  private def listCurationArea(path: Path): List[Path] = {
    managed(Files.list(path)).acquireAndGet(stream => stream.iterator().asScala.toList)
  }

  private def setProperties(depositProperties: PropertiesConfiguration, datamanager: DatamanagerId): Unit = {
    val datamanagerProperties = configuration.datamanagers.getString(datamanager).split(" ")
    depositProperties.setProperty("curation.datamanager.userId", datamanagerProperties(0))
    depositProperties.setProperty("curation.datamanager.email", datamanagerProperties(1))
    depositProperties.save()
  }

  private def clearProperties(depositProperties: PropertiesConfiguration): Unit = {
    depositProperties.clearProperty("curation.datamanager.userId")
    depositProperties.clearProperty("curation.datamanager.email")
    depositProperties.save()
  }

  @tailrec
  private def confirmAction(datamanager: Option[DatamanagerId]): Boolean = {
    StdIn.readLine(s"This action will move all deposits from the curation area of datamanager ${datamanager.getOrElse("")} to the common curation area. OK? (y/n):") match {
      case "y" => true
      case "n" => false
      case _ =>
        println("Please enter a valid char : y or n ")
        confirmAction(datamanager)
    }
  }

  private def depositsFromCurationArea(deposits: List[Path]): Deposits = {
    deposits.filter(Files.isDirectory(_))
      .flatMap { depositDirPath =>
//        debug(s"Getting info from $depositDirPath")
        val depositProperties = new PropertiesConfiguration(depositDirPath.resolve("deposit.properties").toFile)
        val depositorId = depositProperties.getString("depositor.userId")
        val submitted = depositProperties.getString("state.label") == "SUBMITTED"
        val curationRequired = depositProperties.getString("curation.required") == "yes"
        val curationPerformed = depositProperties.getString("curation.performed") == "yes"

        val xml = XML.loadFile(depositDirPath.resolve("bag/metadata/dataset.xml").toFile)
        val titles = xml \\ "title"
        val title = if (titles.nonEmpty) titles.head text else "n/a"
        val audiences = xml \\ "audience"
        val audience = if (audiences.nonEmpty)audiences.head text else "n/a"


        if (submitted  && curationRequired && !curationPerformed) Some {
          Deposit(
            bagId = depositProperties.getString("bag-store.bag-id"),
            title = title,
            depositorId,
            description = depositProperties.getString("state.description"),
            creationTimestamp = Option(depositProperties.getString("creation.timestamp")).getOrElse("n/a"),
            audience = audience
          )
        }
        else
          None
      }
  }

  private def outputCurationList(deposits: Deposits): Unit = {
    val csvFormat: CSVFormat = CSVFormat.RFC4180
      .withHeader("UUID", "TITLE", "DEPOSITOR", "DEPOSIT_CREATION_TIMESTAMP", "AUDIENCE")
      .withDelimiter(',')
      .withRecordSeparator('\n')
    val printer = csvFormat.print(Console.out)
    deposits.sortBy(_.creationTimestamp) foreach { deposit =>
      printer.printRecord(
        deposit.bagId,
        deposit.title,
        deposit.depositor,
        deposit.creationTimestamp,
        deposit.audience)
    }
  }

  private def assignToDatamanager(datamanager: DatamanagerId, personalCurationDirectory: Path, uuid: BagId): String = {
    if (directoryExists(personalCurationDirectory.resolve(uuid))) {
      s"\nError: Deposit $uuid already exists in the personal curation area of datamanager $datamanager."
    } else {
      val depositProperties = new PropertiesConfiguration(commonCurationDir.resolve(uuid).resolve("deposit.properties").toFile)
      setProperties(depositProperties, datamanager)
      FileUtils.moveDirectory(commonCurationDir.resolve(uuid).toFile, personalCurationDirectory.resolve(uuid).toFile)
      s"\nDeposit $uuid has been assigned to datamanager $datamanager."
    }
  }

  private def unassign(datamanager: Option[DatamanagerId], uuid: Option[BagId]): String = {
      val curationDirectory = getCurationDirectory(datamanager)
      if (directoryExists(curationDirectory, uuid)) {
        if (uuid.isEmpty && !confirmAction(datamanager))
          s"\nAction cancelled"
        else
          unassignFromDatamanager(curationDirectory, uuid, datamanager)
       }
      else
        uuid match {
          case Some(_) => s"\nError: Deposit ${uuid.getOrElse("")} not found in the curation area of datamanager ${datamanager.getOrElse("")}."
          case None => s"\nError: No personal curation area found for datamanager ${datamanager.getOrElse("")}."
      }
  }

  private def unassignFromDatamanager(personalCurationDirectory: Path, uuid: Option[BagId], datamanager: Option[DatamanagerId]): String = {
    uuid match {
      case Some(_) => unassignDeposit(personalCurationDirectory.resolve(uuid.getOrElse("")), datamanager.getOrElse(""))
      case None => var msg = "";
        Files.list(personalCurationDirectory).filter(Files.isDirectory(_)).
          forEach(deposit => msg += unassignDeposit(deposit, datamanager.getOrElse("")))
        if (msg.isEmpty) msg = s"There were no deposits to unassign."
        msg
    }
  }

  private def unassignDeposit(deposit: Path, datamanager: DatamanagerId): String = {
    val depositProperties = new PropertiesConfiguration(deposit.resolve("deposit.properties").toFile)
    if (!isSubmitted(depositProperties) || isCurated(depositProperties)) {
      var msg = ""
      if (!isSubmitted(depositProperties))
        msg = s"\nError: Deposit $deposit is not SUBMITTED. It was not unassigned."
      if (isCurated(depositProperties))
        msg += s"\nError: Deposit $deposit has already been curated. It was not unassigned."
      msg
    }
    else {
      clearProperties(depositProperties)
      FileUtils.moveDirectory(deposit.toFile, commonCurationDir.resolve(deposit.getFileName).toFile)
      s"\nDeposit $deposit has been unassigned from datamanager $datamanager."
    }
  }

  def listCurationWork(datamanager: Option[DatamanagerId] = None): Try[String] = Try {
    val curationDirectory = getCurationDirectory(datamanager)
    if (directoryExists(curationDirectory)) {
      outputCurationList(depositsFromCurationArea(listCurationArea(curationDirectory)))
      s"\nEnd of curation list."
    }
    else  s"\nError: No personal curation area found for datamanager ${datamanager.getOrElse("")}."
  }

  def assignCurationWork(datamanager: Option[DatamanagerId] = None, uuid: Option[BagId] = None): Try[String] = Try {
    val curationDirectory = getCurationDirectory(datamanager)
    if (directoryExists(commonCurationDir, uuid)) {
      if (directoryExists(curationDirectory))
        assignToDatamanager(datamanager.getOrElse(""), curationDirectory, uuid.getOrElse(""))
      else  s"\nError: No personal curation area found for datamanager ${datamanager.getOrElse("")}."
    }
    else s"\nError: Deposit ${uuid.getOrElse("")} not found in the common curation area."
  }

  def unassignCurationWork(datamanager: Option[DatamanagerId] = None, uuid: Option[BagId] = None): Try[String] = Try {
    datamanager match {
      case Some(_) => unassign(datamanager, uuid)
      case None => unassign(getCurrentUnixUser, uuid)
    }
  }

}
