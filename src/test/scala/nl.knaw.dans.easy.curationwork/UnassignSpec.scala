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
import org.apache.commons.configuration.PropertiesConfiguration

import scala.language.postfixOps
import scala.util.Success

class UnassignSpec extends TestSupportFixture {

  val resourceDir = File(getClass.getResource("/"))
  val datamanagerProperties = new Configuration("version x.y.z",
    new PropertiesConfiguration() {},
    new PropertiesConfiguration() {
      setDelimiterParsingDisabled(true)
      load(resourceDir / "debug-config" / "datamanager.properties" toJava)
    }).datamanagers

  val commonCurationArea = testDir / "easy-common-curation-area"
  val datamanagerCurationAreas = testDir / "datamanager-curation-areas"
  val managerCurationDirString = datamanagerCurationAreas / "$unix-user/curation-area" toString
  val jannekesCurationArea = datamanagerCurationAreas / "janneke/curation-area"

  val assigner = new Assign(commonCurationArea, managerCurationDirString, datamanagerProperties)
  val unassigner = new Unassign(commonCurationArea, managerCurationDirString)

  val janneke = "janneke"
  val jip = "jip"
  val bagId = "38bc40f9-12d7-42c6-808a-8eac77bfc726"

  override def beforeEach(): Unit = {
    commonCurationArea delete (true)
    jannekesCurationArea delete (true)
    File(getClass.getResource("/easy-common-curation-area")) copyTo commonCurationArea
    jannekesCurationArea.createDirectories()
    commonCurationArea.toJava should exist
    jannekesCurationArea.toJava should exist
  }

  "unassign from an existing datamanager with an existing bagId (in the personal curation area)" should "succeed" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(bagId)).getOrElse("") should include(s"$bagId has been unassigned from datamanager $janneke")
  }

  "after unassigning a deposit, deposit properties" should "not anymore contain curator properties" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea / bagId / "deposit.properties" toJava)
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.userId").toString should include("user001")
    depositPropertiesInPersonalCurationArea.getProperty("curation.datamanager.email").toString should include("janneke@dans.knaw.nl")

    unassigner.unassignCurationWork(Some(janneke), Some(bagId)) shouldBe a[Success[_]]
    val depositPropertiesInCommonCurationArea = new PropertiesConfiguration(commonCurationArea / bagId / "deposit.properties" toJava)
    depositPropertiesInCommonCurationArea.getProperty("curation.datamanager.userId") shouldBe null
    depositPropertiesInCommonCurationArea.getProperty("curation.datamanager.email") shouldBe null
  }

  "unassigning a bagId that does not (anymore) exist in the personal curation area of a datamanager" should "fail" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(bagId)) shouldBe a[Success[_]]
    unassigner.unassignCurationWork(Some(janneke), Some(bagId)).getOrElse("") should include(s"There were no deposits in the personal curation area of $janneke to unassign, starting with $bagId")
  }

  "unassigning all deposits from a datamanager who does not yet have a personal curation area" should "fail" in {
    unassigner.unassignCurationWork(Some(jip), None).getOrElse("") should include(s"No personal curation area found for datamanager $jip")
  }

  "unassigning a deposit that is not in state 'submitted'" should "fail" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea / bagId / "deposit.properties" toJava)
    depositPropertiesInPersonalCurationArea.setProperty("state.label", "NOT SUBMITTED")
    depositPropertiesInPersonalCurationArea.save()

    unassigner.unassignCurationWork(Some(janneke), Some(bagId)).getOrElse("") should include(s"$bagId is not SUBMITTED. It was not unassigned")
  }

  "unassigning a deposit that is in state 'curation performed'" should "fail" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    val depositPropertiesInPersonalCurationArea = new PropertiesConfiguration(jannekesCurationArea / bagId / "deposit.properties" toJava)
    depositPropertiesInPersonalCurationArea.setProperty("curation.performed", "yes")
    depositPropertiesInPersonalCurationArea.save()

    unassigner.unassignCurationWork(Some(janneke), Some(bagId)).getOrElse("") should include(s"$bagId has already been curated. It was not unassigned")
  }
}
