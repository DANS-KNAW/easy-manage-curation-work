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

import java.nio.file.{ Files, Paths }

import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.io.FileUtils

import scala.util.Success

class AssignSpec extends TestSupportFixture {

  val resourceDirString: String = Paths.get(getClass.getResource("/").toURI).toAbsolutePath.toString
  val datamanagerProperties = new Configuration("version x.y.z",
    new PropertiesConfiguration() {},
    new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load(Paths.get(resourceDirString + "/debug-config", "datamanager.properties").toFile)
    }).datamanagers

  val commonCurationArea = testDir.resolve("easy-common-curation-area")
  val datamanagerCurationAreas = testDir.resolve("datamanager-curation-areas")
  val managerCurationDirString = datamanagerCurationAreas.resolve("$unix-user/curation-area").toString
  val jannekesCurationArea = datamanagerCurationAreas.resolve("janneke/curation-area")

  val assigner = new Assign(commonCurationArea, managerCurationDirString, datamanagerProperties)

  val janneke ="janneke"
  val jip ="jip"
  val bagId = "38bc40f9-12d7-42c6-808a-8eac77bfc726"


  override def beforeEach(): Unit = {
    FileUtils.copyDirectory(Paths.get(getClass.getResource("/easy-common-curation-area").toURI).toFile, commonCurationArea.toFile)
    FileUtils.deleteQuietly(jannekesCurationArea.toFile)
    Files.createDirectories(jannekesCurationArea)
    commonCurationArea.toFile should exist
    jannekesCurationArea.toFile should exist
  }

  "assign to existing datamanager with an existing bagId (in the common curation area)" should "succeed" in {
    assigner.assignCurationWork(janneke, bagId).getOrElse("") should include(s"$bagId has been assigned to datamanager $janneke")
  }

  "deposit properties" should "after assignment contain curation properties of the datamanager" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]

    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea.resolve(bagId).resolve("deposit.properties").toFile)
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.userId").toString should include("user001")
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.email").toString should include("janneke@dans.knaw.nl")
  }

  "assigning twice with the same parameters" should "fail" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    assigner.assignCurationWork(janneke, bagId).getOrElse("") should include(s"Deposit $bagId not found in the common curation area")
  }

  "assigning non-existing bagId" should "fail" in {
    assigner.assignCurationWork(janneke, "non-existing-bagId").getOrElse("") should include(s"Deposit non-existing-bagId not found in the common curation area")
  }

  "assigning a bagId that already exists in the personal curation area of a datamanager" should "fail" in {
    FileUtils.copyDirectory(commonCurationArea.resolve(bagId).toFile, jannekesCurationArea.resolve(bagId).toFile)
    assigner.assignCurationWork(janneke, bagId).getOrElse("") should include(s"Deposit $bagId already exists in the personal curation area of datamanager $janneke")
  }

  "assigning to a datamanager who does not yet have a personal curation area" should "fail" in {
    assigner.assignCurationWork(jip, bagId).getOrElse("") should include(s"No personal curation area found for datamanager $jip")
  }

}
