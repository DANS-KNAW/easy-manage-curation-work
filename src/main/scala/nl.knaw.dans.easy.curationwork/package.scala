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
package nl.knaw.dans.easy

import java.nio.file.Paths

package object curationwork {

  type Deposits = Seq[Deposit]
  type BagId = String
  type DepositorId = String
  type DatamanagerId = String

  val configuration = Configuration(Paths.get(System.getProperty("app.home")))
  val commonCurationDir = Paths.get(configuration.properties.getString("curation.common.directory"))
  val managerCurationDirString = configuration.properties.getString("curation.personal.directory")
}
