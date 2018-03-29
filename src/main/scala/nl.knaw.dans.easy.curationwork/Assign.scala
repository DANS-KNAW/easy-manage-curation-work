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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.csv.CSVFormat
import org.apache.commons.io.FileUtils
import resource.managed

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.language.postfixOps
import scala.sys.process._
import scala.util.Try
import scala.xml.XML


class Assign(configuration: Configuration) extends DebugEnhancedLogging {

  private def getCurationDirectory(datamanager: Option[DatamanagerId]): Path = {
    datamanager.map(getManagerCurationDir).getOrElse(commonCurationDir)
  }

  private def getManagerCurationDir(datamanager: DatamanagerId): Path = {
    Paths.get(managerCurationDirString.replace("$unix-user", datamanager))
  }

  private def directoryExists(dir: Path, uuid: Option[BagId] = None): Boolean = {
    Files.exists(dir.resolve(uuid.getOrElse("")))
  }

  private def setProperties(depositProperties: PropertiesConfiguration, datamanager: DatamanagerId): Unit = {
    val datamanagerProperties = configuration.datamanagers.getString(datamanager).split(" ")
    depositProperties.setProperty("curation.datamanager.userId", datamanagerProperties(0))
    depositProperties.setProperty("curation.datamanager.email", datamanagerProperties(1))
    depositProperties.save()
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

  def assignCurationWork(datamanager: DatamanagerId, uuid: BagId): Try[String] = Try {
    val curationDirectory = getCurationDirectory(Some(datamanager))
    if (directoryExists(commonCurationDir, Some(uuid))) {
      if (directoryExists(curationDirectory))
        assignToDatamanager(datamanager, curationDirectory, uuid)
      else s"\nError: No personal curation area found for datamanager $datamanager."
    }
    else s"\nError: Deposit $uuid not found in the common curation area."
  }
}
