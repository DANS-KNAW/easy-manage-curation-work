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

class CurationWorkSpec extends TestSupportFixture {

  val commonCurationArea = testDir.resolve("easy-common-curation-area")
  val datamanagerCurationAreas = testDir.resolve("datamanager-curation-areas")
  val managerCurationDirString = datamanagerCurationAreas.resolve("$unix-user/curation-area").toString

  val app = new EasyManageCurationWorkApp(commonCurationArea, managerCurationDirString)
  val janneke ="janneke"

  "getCurationDirectory" should "return correct path to the personal curation area of a datamanager" in {
    app.getCurationDirectory(Some(janneke)) shouldBe datamanagerCurationAreas.resolve(managerCurationDirString.replace("$unix-user", janneke))
  }

  "getCurationDirectory without datamanager parameter" should "return correct path to the common curation area" in {
    app.getCurationDirectory(None) shouldBe commonCurationArea
  }

}
