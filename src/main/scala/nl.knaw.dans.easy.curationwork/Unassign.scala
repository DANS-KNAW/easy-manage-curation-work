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

import java.nio.file.{Files, Path}

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.io.FileUtils
import resource.managed

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Try

class Unassign(configuration: Configuration) extends EasyManageCurationWorkApp(configuration) with DebugEnhancedLogging {

  private def getCurrentUnixUser: String = {
    "whoami" !!
  }

  private def isSubmitted(depositProperties: PropertiesConfiguration): Boolean = {
    depositProperties.getString("state.label") == "SUBMITTED"
  }

  private def isCurated(depositProperties: PropertiesConfiguration): Boolean = {
    depositProperties.getString("curation.performed") == "yes"
  }

  private def clearProperties(depositProperties: PropertiesConfiguration): Unit = {
    depositProperties.clearProperty("curation.datamanager.userId")
    depositProperties.clearProperty("curation.datamanager.email")
    depositProperties.save()
  }

  @tailrec
  private def confirmAction(datamanager: DatamanagerId): Boolean = {
    StdIn.readLine(s"This action will move all deposits from the curation area of datamanager $datamanager to the common curation area. OK? (y/n):") match {
      case "y" => true
      case "n" => false
      case _ =>
        println("Please enter a valid char : y or n ")
        confirmAction(datamanager)
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

  private def unassignFromDatamanager(personalCurationDirectory: Path, uuid: Option[BagId], datamanager: DatamanagerId): String = {
    uuid match {
      case Some(deposit) => unassignDeposit(personalCurationDirectory.resolve(deposit), datamanager)
      case None =>
        val msg = managed(Files.list(personalCurationDirectory)).acquireAndGet(jStream => jStream.iterator().asScala.toList)
          .filter(Files.isDirectory(_))
          .foldLeft("")((msg, deposit) => msg + unassignDeposit(deposit, datamanager))

        if (msg.isEmpty) "There were no deposits to unassign."
        else msg
    }
  }

  private def unassign(datamanager: DatamanagerId, uuid: Option[BagId]): String = {
    val curationDirectory = getCurationDirectory(Some(datamanager))
    if (directoryExists(curationDirectory, uuid)) {
      if (uuid.isEmpty && !confirmAction(datamanager))
        s"\nAction cancelled"
      else
        unassignFromDatamanager(curationDirectory, uuid, datamanager)
    }
    else
      uuid match {
        case Some(deposit) => s"\nError: Deposit $deposit not found in the curation area of datamanager $datamanager."
        case None => s"\nError: No personal curation area found for datamanager $datamanager."
      }
  }

  def unassignCurationWork(datamanager: Option[DatamanagerId] = None, uuid: Option[BagId] = None): Try[String] = Try {
    datamanager match {
      case Some(dm) => unassign(dm, uuid)
      case None => unassign(getCurrentUnixUser, uuid)
    }
  }
}
