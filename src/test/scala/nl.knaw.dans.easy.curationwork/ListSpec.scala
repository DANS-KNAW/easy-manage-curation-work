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

class ListSpec extends TestSupportFixture {

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
  val reporter = new Report(commonCurationArea, managerCurationDirString)

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

  "list without datamanager parameter" should "list four deposits" in {
    reporter.depositsFromCurationArea(reporter.listCurationArea(commonCurationArea)).toList should have size 4
  }

  "it" should "list three deposits after assigning a deposit to a datamanager" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    reporter.depositsFromCurationArea(reporter.listCurationArea(commonCurationArea)).toList should have size 3
  }

  "it" should "list one deposits in the personal curation area of the datamanager to whom the deposit was assigned" in {
    assigner.assignCurationWork(janneke, bagId) shouldBe a[Success[_]]
    reporter.depositsFromCurationArea(reporter.listCurationArea(jannekesCurationArea)).toList should have size 1
  }

  "listing deposits of a datamanager who does not yet have personal curation area" should "fail" in {
    reporter.listCurationWork(Some(jip)).getOrElse("") should include(s"No personal curation area found for datamanager $jip")
  }

  "listing the common curation area" should s"contain details about deposit $bagId" in {
    val deposit = reporter.depositsFromCurationArea(reporter.listCurationArea(commonCurationArea)).filter(deposit => deposit.bagId.equals(bagId)).head
    deposit.bagId shouldBe bagId
    deposit.title shouldBe "Reis naar Centaur-planetoïde"
    deposit.depositor shouldBe "user001"
    deposit.creationTimestamp shouldBe "2018-02-27T11:32:42.204Z"
    deposit.audience shouldBe "D30000"
  }
}
