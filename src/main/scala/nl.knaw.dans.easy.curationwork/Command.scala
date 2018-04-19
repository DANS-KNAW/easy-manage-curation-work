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
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.control.NonFatal
import scala.util.{ Failure, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  val configuration = Configuration(File(System.getProperty("app.home")))
  val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }
  val commonCurationArea = File(configuration.properties.getString("curation.common.directory"))
  val managerCurationDirString = configuration.properties.getString("curation.personal.directory")
  val datamanagerProperties = configuration.datamanagers

  val reporter = new Report(commonCurationArea, managerCurationDirString)
  val assigner = new Assign(commonCurationArea, managerCurationDirString, datamanagerProperties)
  val unassigner = new Unassign(commonCurationArea, managerCurationDirString)

  runSubcommand()
    .doIfSuccess(msg => println(s"$msg"))
    .doIfFailure { case e => logger.error(e.getMessage, e) }
    .doIfFailure { case NonFatal(e) => println(s"FAILED: ${ e.getMessage }") }

  private def runSubcommand(): Try[FeedBackMessage] = {
    if (!commonCurationAreaExists)
      Try(s"Error: No common curation area found.")
    else
      commandLine.subcommand
        .collect {
          case cmd @ commandLine.list =>
            if (validDatamanager(cmd.datamanager.toOption)) reporter.listCurationWork(cmd.datamanager.toOption)
            else Try(s"Error: Unknown datamanager ${ cmd.datamanager() } (missing in datamanager properties file)")
          case cmd @ commandLine.assign =>
            if (userIdAndEmailExist(cmd.datamanager())) assigner.assignCurationWork(cmd.datamanager(), cmd.bagId())
            else Try(s"Error: Easy-userid and/or email address of datamanager ${ cmd.datamanager() } missing in datamanager properties file")
          case cmd @ commandLine.unassign =>
            if (validDatamanager(cmd.datamanager.toOption)) unassigner.unassignCurationWork(cmd.datamanager.toOption, cmd.bagId.toOption)
            else Try(s"Unknown datamanager ${ cmd.datamanager() } (missing in datamanager properties file)")
        }
        .getOrElse(Failure(new IllegalArgumentException(s"Unknown command: ${ commandLine.subcommand }")))
  }

  private def commonCurationAreaExists(): Boolean = {
    commonCurationArea.exists
  }

  private def userIdAndEmailExist(datamanager: DatamanagerId): Boolean = {
    datamanagerProperties.containsKey(datamanager + EASY_USER_ID_SUFFIX) && datamanagerProperties.containsKey(datamanager + EMAIL_SUFFIX)
  }

  private def validDatamanager(datamanager: Option[DatamanagerId]): Boolean = {
    datamanager.isEmpty || datamanagerProperties.containsKey(datamanager.get + EASY_USER_ID_SUFFIX)
  }
}
