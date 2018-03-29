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

import sys.process._
import org.apache.commons.configuration.PropertiesConfiguration
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import resource.managed
import org.apache.commons.csv.CSVFormat
import org.apache.commons.io.FileUtils

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.Try
import scala.xml.XML


class Report(configuration: Configuration) extends DebugEnhancedLogging {

  private def getCurationDirectory(datamanager: Option[DatamanagerId]): Path = {
    datamanager.map(getManagerCurationDir).getOrElse(commonCurationDir)
  }

  private def getManagerCurationDir(datamanager: DatamanagerId): Path = {
    Paths.get(managerCurationDirString.replace("$unix-user", datamanager))
  }

  private def listCurationArea(path: Path): List[Path] = {
    managed(Files.list(path)).acquireAndGet(stream => stream.iterator().asScala.toList)
  }

  private def depositsFromCurationArea(deposits: List[Path]): Deposits = {
    deposits.filter(Files.isDirectory(_))
      .flatMap { depositDirPath =>
        //        debug(s"Getting info from $depositDirPath")
        val depositProperties = new PropertiesConfiguration(depositDirPath.resolve("deposit.properties").toFile)
        val depositorId = depositProperties.getString("depositor.userId")
        val submitted = depositProperties.getString("state.label") == "SUBMITTED"
        val curationRequired = depositProperties.getString("curation.required") == "yes"
        val curationPerformed = depositProperties.getString("curation.performed") == "yes"

        if (submitted && curationRequired && !curationPerformed) Some {

          // get the bag directory; it is expected that there is exactly one directory to be found
          val bagDir = managed(Files.list(depositDirPath)).acquireAndGet(stream => stream.iterator().asScala.toList).filter(Files.isDirectory(_)).head

          val xml = XML.loadFile(bagDir.resolve("metadata/dataset.xml").toFile)
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

  private def outputCurationList(deposits: Deposits): Unit = {
    val csvFormat: CSVFormat = CSVFormat.RFC4180
      .withHeader("UUID", "TITLE", "DEPOSITOR", "DEPOSIT_CREATION_TIMESTAMP", "AUDIENCE")
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
    if (Files.exists((curationDirectory))) {
      outputCurationList(depositsFromCurationArea(listCurationArea(curationDirectory)))
      s"\nEnd of curation list."
    }
    else s"\nError: No personal curation area found for datamanager ${datamanager.getOrElse("")}."
  }
}
