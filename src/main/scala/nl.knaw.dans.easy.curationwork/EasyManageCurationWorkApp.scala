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
import scala.language.postfixOps


class EasyManageCurationWorkApp(configuration: Configuration) extends DebugEnhancedLogging {

  val commonCurationDir = Paths.get(configuration.properties.getString("curation.common.directory"))
  val managerCurationDirString = configuration.properties.getString("curation.personal.directory")

  def getCurationDirectory(datamanager: Option[DatamanagerId]): Path = {
    datamanager.map(getManagerCurationDir).getOrElse(commonCurationDir)
  }

  private def getManagerCurationDir(datamanager: DatamanagerId): Path = {
    Paths.get(managerCurationDirString.replace("$unix-user", datamanager))
  }

  def directoryExists(dir: Path, uuid: Option[BagId] = None): Boolean = {
    Files.exists(dir.resolve(uuid.getOrElse("")))
  }
}
