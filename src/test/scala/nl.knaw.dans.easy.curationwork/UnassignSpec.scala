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

import java.io.FileNotFoundException
import java.nio.file.{ Files, Paths }

import org.apache.commons.configuration.PropertiesConfiguration
import org.apache.commons.io.FileUtils

import scala.util.Success

class UnassignSpec extends TestSupportFixture {

  val configuration = Configuration(Paths.get("home"))
  val datamanagerProperties = configuration.datamanagers

  val commonCurationArea = testDir.resolve("easy-common-curation-area")
  val datamanagerCurationAreas = testDir.resolve("datamanager-curation-areas")
  val managerCurationDirString = datamanagerCurationAreas.resolve("$unix-user/curation-area").toString
  val jannekesCurationArea = datamanagerCurationAreas.resolve("janneke/curation-area")

  val assigner = new Assign(commonCurationArea, managerCurationDirString, datamanagerProperties)
  val unassigner = new Unassign(commonCurationArea, managerCurationDirString)
  val reporter = new Report(commonCurationArea, managerCurationDirString)

  val janneke ="janneke"
  val jip ="jip"
  val uuid = "38bc40f9-12d7-42c6-808a-8eac77bfc726"
  val uuid2 = "48bc40f9-12d7-42c6-808a-8eac77bfc726"


  override def beforeEach(): Unit = {
    FileUtils.copyDirectory(Paths.get(getClass.getResource("/easy-common-curation-area").toURI).toFile, commonCurationArea.toFile)
    FileUtils.deleteQuietly(jannekesCurationArea.toFile)
    Files.createDirectories(jannekesCurationArea)
    commonCurationArea.toFile should exist
    jannekesCurationArea.toFile should exist
  }

  "unassign from an existing datamanager with an existing uuid (in the personal curation area)" should "succeed" in {
    assigner.assignCurationWork(janneke, uuid) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(uuid)).getOrElse("") should include(s"$uuid has been unassigned from datamanager $janneke")
  }

  "after unassigning a deposit, deposit properties" should "not anymore contain curator properties" in {
    assigner.assignCurationWork(janneke, uuid) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea.resolve(uuid).resolve("deposit.properties").toFile)
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.userId").toString should include("user001")
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.email").toString should include("janneke@dans.knaw.nl")

    unassigner.unassignCurationWork(Some(janneke), Some(uuid)) shouldBe a[Success[_]]
    val depositPropertiesInCommonCurationArea = new PropertiesConfiguration(commonCurationArea.resolve(uuid).resolve("deposit.properties").toFile)
    depositPropertiesInCommonCurationArea.getProperty("curation.datamanager.userId") shouldBe null
    depositPropertiesInCommonCurationArea.getProperty("curation.datamanager.email") shouldBe null
  }

  "unassigning a uuid that does not (anymore) exist in the personal curation area of a datamanager" should "fail" in {
    assigner.assignCurationWork(janneke, uuid) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(uuid)) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(uuid)).getOrElse("") should include(s"$uuid not found in the curation area of datamanager $janneke")
  }

  "unassigning all deposits from a datamanager who does not yet have a personal curation area" should "fail" in {
    unassigner.unassignCurationWork(Some(jip), None).getOrElse("") should include(s"No personal curation area found for datamanager $jip")
  }

  "unassigning a deposit that is not in state 'submitted'" should "fail" in {
    assigner.assignCurationWork(janneke, uuid) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea.resolve(uuid).resolve("deposit.properties").toFile)
    depositPropertiesInPersonalCurationArea.setProperty("state.label", "NOT SUBMITTED")
    depositPropertiesInPersonalCurationArea.save()

    unassigner.unassignCurationWork(Some(janneke), Some(uuid)).getOrElse("") should include(s"$uuid is not SUBMITTED. It was not unassigned")
  }

  "unassigning a deposit that is in state 'curation performed'" should "fail" in {
    assigner.assignCurationWork(janneke, uuid) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea.resolve(uuid).resolve("deposit.properties").toFile)
    depositPropertiesInPersonalCurationArea.setProperty("curation.performed", "yes")
    depositPropertiesInPersonalCurationArea.save()

    unassigner.unassignCurationWork(Some(janneke), Some(uuid)).getOrElse("") should include(s"$uuid has already been curated. It was not unassigned")
  }

}
