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

import java.nio.file.{ Files, Path }

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.io.FileUtils

import scala.language.postfixOps
import scala.util.Try

class Assign(commonCurationDir: Path, managerCurationDirString: String, datamanagerProperties: PropertiesConfiguration) extends EasyManageCurationWorkApp(commonCurationDir, managerCurationDirString) with DebugEnhancedLogging  {

  private def setProperties(depositProperties: PropertiesConfiguration, datamanager: String): Unit = {
    val Array(userId, email) = datamanagerProperties.getString(datamanager).split(" ", 2)
    depositProperties.setProperty("curation.datamanager.userId", userId)
    depositProperties.setProperty("curation.datamanager.email", email)
    depositProperties.save()
  }

  private def assignToDatamanager(datamanager: DatamanagerId, personalCurationDirectory: Path, uuid: Uuid): String = {
    if (Files.exists(personalCurationDirectory.resolve(uuid))) {
      s"\nError: Deposit $uuid already exists in the personal curation area of datamanager $datamanager."
    } else {
      val depositProperties = new PropertiesConfiguration(commonCurationDir.resolve(uuid).resolve("deposit.properties").toFile)
      setProperties(depositProperties, datamanager)
      FileUtils.moveDirectory(commonCurationDir.resolve(uuid).toFile, personalCurationDirectory.resolve(uuid).toFile)
      s"\nDeposit $uuid has been assigned to datamanager $datamanager."
    }
  }

  def assignCurationWork(datamanager: DatamanagerId, uuid: Uuid): Try[String] = Try {
    val curationDirectory = getCurationDirectory(Some(datamanager))
    if (Files.exists(commonCurationDir.resolve(uuid))) {
      if (Files.exists(curationDirectory))
        assignToDatamanager(datamanager, curationDirectory, uuid)
      else s"\nError: No personal curation area found for datamanager $datamanager."
    }
    else s"\nError: Deposit $uuid not found in the common curation area."
  }
}
