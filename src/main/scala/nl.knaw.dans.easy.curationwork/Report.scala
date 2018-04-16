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
import org.apache.commons.csv.CSVFormat

import scala.language.postfixOps
import scala.util.Try
import scala.xml.XML


class Report(commonCurationDir: File, managerCurationDirString: String) extends EasyManageCurationWorkApp(commonCurationDir, managerCurationDirString) with DebugEnhancedLogging {

  def listCurationArea(dir: File): List[File] = {
    dir.list.toList
  }

  def depositsFromCurationArea(deposits: List[File]): Deposits = {
    deposits.filter(_.isDirectory)
      .flatMap { depositDir =>
        //        debug(s"Getting info from $depositDir")
        val depositProperties = new PropertiesConfiguration(depositDir / "deposit.properties" toJava)
        val depositorId = depositProperties.getString("depositor.userId")
        val submitted = depositProperties.getString("state.label") == "SUBMITTED"
        val curationRequired = depositProperties.getString("curation.required") == "yes"
        val curationPerformed = depositProperties.getString("curation.performed") == "yes"

        if (submitted && curationRequired && !curationPerformed) Some {

          // get the bag directory; it is expected that there is exactly one directory to be found
          val bagDir = depositDir.list.toList.filter(_.isDirectory).head

          val xml = XML.loadFile(bagDir / "metadata/dataset.xml" toJava)
          val titles = xml \\ "title"
          val title = titles.headOption.map(_.text).getOrElse("n/a").toString
          val audiences = xml \\ "audience"
          val audience = audiences.headOption.map(_.text).getOrElse("n/a").toString


          Deposit(
            bagId = depositProperties.getString("bag-store.bag-id"),
            title = title,
            depositorId,
            description = depositProperties.getString("state.description"),
            creationTimestamp = Option(depositProperties.getString("creation.timestamp")).getOrElse("n/a"),
            audience = audience
          )
        }
        else
          None
      }
  }

  def outputCurationList(deposits: Deposits): Unit = {
    val csvFormat: CSVFormat = CSVFormat.RFC4180
      .withHeader("BAG_ID", "TITLE", "DEPOSITOR", "DEPOSIT_CREATION_TIMESTAMP", "AUDIENCE")
      .withDelimiter(',')
      .withRecordSeparator('\n')
    val printer = csvFormat.print(Console.out)
    deposits.sortBy(_.creationTimestamp) foreach { deposit =>
      printer.printRecord(
        deposit.bagId,
        deposit.title,
        deposit.depositor,
        deposit.creationTimestamp,
        deposit.audience)
    }
  }

  def listCurationWork(datamanager: Option[DatamanagerId] = None): Try[String] = Try {
    val curationDirectory = getCurationDirectory(datamanager)
    if (curationDirectory exists) {
      outputCurationList(depositsFromCurationArea(listCurationArea(curationDirectory)))
      s"\nEnd of curation list."
    }
    else s"\nError: No personal curation area found for datamanager ${ datamanager.getOrElse("") }."
  }
}
