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

import scala.language.postfixOps
import scala.util.Try

class Assign(commonCurationDir: File, managerCurationDirString: String, datamanagerProperties: PropertiesConfiguration) extends EasyManageCurationWorkApp(commonCurationDir, managerCurationDirString) with DebugEnhancedLogging {

  private def setProperties(depositProperties: PropertiesConfiguration, datamanager: String): Unit = {
    val userId = datamanagerProperties.getString(datamanager + EASY_USER_ID_SUFFIX)
    val email = datamanagerProperties.getString(datamanager + EMAIL_SUFFIX)
    depositProperties.setProperty("curation.datamanager.userId", userId)
    depositProperties.setProperty("curation.datamanager.email", email)
    depositProperties.save()
  }

  private def assignToDatamanager(datamanager: DatamanagerId, personalCurationDirectory: File, bagId: BagId): String = {
    if (personalCurationDirectory / bagId exists) {
      s"\nError: Deposit $bagId already exists in the personal curation area of datamanager $datamanager."
    }
    else {
      val depositProperties = new PropertiesConfiguration((commonCurationDir / bagId / "deposit.properties").toJava)
      setProperties(depositProperties, datamanager)
      commonCurationDir / bagId moveTo personalCurationDirectory / bagId
      s"\nDeposit $bagId has been assigned to datamanager $datamanager."
    }
  }

  def assignCurationWork(datamanager: DatamanagerId, bagId: BagId): Try[String] = Try {
    val curationDirectory = getCurationDirectory(Some(datamanager))
    if (commonCurationDir / bagId exists) {
      if (curationDirectory exists)
        assignToDatamanager(datamanager, curationDirectory, bagId)
      else s"\nError: No personal curation area found for datamanager $datamanager."
    }
    else s"\nError: Deposit $bagId not found in the common curation area."
  }
}
