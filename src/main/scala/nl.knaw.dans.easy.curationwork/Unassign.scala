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

import better.files.File
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration

import scala.annotation.tailrec
import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Try

class Unassign(commonCurationDir: File, managerCurationDirString: String) extends EasyManageCurationWorkApp(commonCurationDir, managerCurationDirString) with DebugEnhancedLogging {

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

  @tailrec
  private def confirmUnassigningMoreThanOneDeposit(datamanager: DatamanagerId, bagIds: List[File]): Boolean = {
    StdIn.readLine(s"This action will move deposits ${bagIds.map(deposit => deposit.name).mkString("[", ", ", "]")} from the curation area of $datamanager to the common curation area. OK? (y/n):") match {
      case "y" => true
      case "n" => false
      case _ =>
        println("Please enter a valid char : y or n ")
        confirmUnassigningMoreThanOneDeposit(datamanager, bagIds)
    }
  }

  private def unassignDeposit(deposit: File, datamanager: DatamanagerId): String = {
    val depositProperties = new PropertiesConfiguration(deposit / "deposit.properties" toJava)
    if (!isSubmitted(depositProperties) || isCurated(depositProperties)) {
      var msg = ""
      if (!isSubmitted(depositProperties))
        msg = s"\nError: Deposit $deposit is not SUBMITTED. It was not unassigned"
      if (isCurated(depositProperties))
        msg += s"\nError: Deposit $deposit has already been curated. It was not unassigned"
      msg
    }
    else {
      clearProperties(depositProperties)
      deposit moveTo commonCurationDir / deposit.name
      s"\nDeposit ${deposit.name} has been unassigned from datamanager $datamanager"
    }
  }

  private def unassignFromDatamanager(personalCurationDirectory: File, bagId: Option[BagId], datamanager: DatamanagerId): String = {
    // It is possible to give just part of the bag-id, and then all deposits starting with that part are unassigned
    // If no bag-id is given, then all deposits of the datamanager are unassigned
    val bagIds = bagId match {
      case Some(deposit) => personalCurationDirectory.list.toList.filter(file => file.isDirectory && file.name.startsWith(deposit))
      case None => personalCurationDirectory.list.toList.filter(_ isDirectory)
    }
    if (bagIds.isEmpty)
      s"There were no deposits in the personal curation area of $datamanager to unassign, starting with ${bagId.getOrElse("")}"
    else {
      if (bagIds.size > 1  && !confirmUnassigningMoreThanOneDeposit(datamanager, bagIds))
        s"Action cancelled"
      else
        bagIds.foldLeft("")((msg, deposit) => msg + unassignDeposit(deposit, datamanager))
    }
  }

  private def unassign(datamanager: DatamanagerId, bagId: Option[BagId]): String = {
    val curationDirectory = getCurationDirectory(Some(datamanager))
    if (bagId.isEmpty && !confirmAction(datamanager))
      s"Action cancelled"
    else if (curationDirectory notExists)
      s"Error: No personal curation area found for datamanager $datamanager"
    else
      unassignFromDatamanager(curationDirectory, bagId, datamanager)
  }

  def unassignCurationWork(datamanager: Option[DatamanagerId] = None, bagId: Option[BagId] = None): Try[String] = Try {
    datamanager match {
      case Some(dm) => unassign(dm, bagId)
      case None => unassign(getCurrentUnixUser, bagId)
    }
  }
}
