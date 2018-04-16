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

import org.rogach.scallop.{ ScallopConf, ScallopOption, Subcommand }

class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "easy-manage-curation-work"
  version(configuration.version)
  private val SUBCOMMAND_SEPARATOR = "---\n"
  val description: String = s"""View and assign curation tasks"""
  val synopsis: String =
    s"""
       |  $printedName list [<easy-datamanager>]
       |  $printedName assign <easy-datamanager> <bag-id>
       |  $printedName unassign [<easy-datamanager> [<bag-id>]]
     """.stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |  $description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)

  val list = new Subcommand("list") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", descr = "Datamanager, whose to-be-curated deposits are listed. If not specified, deposits from the common curation area are listed.", required = false)
    descr("Lists the current curation tasks.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(list)

  val assign = new Subcommand("assign") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", descr = "Datamanager, to whom the deposit will be assigned.")
    val bagId: ScallopOption[DatamanagerId] = trailArg("bag-id", descr = "bag id of the deposit to be assigned.")
    descr("Assigns curation task to a datamanager.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(assign)

  val unassign = new Subcommand("unassign") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", descr = "Datamanager, from whom the deposit will be unassigned. If not specified, datamanager is the current user.", required = false)
    val bagId: ScallopOption[DatamanagerId] = trailArg("bag-id", descr = "bag id of the deposit to be unassigned. If not specified, all deposits of the datamanager are unassigned.", required = false)
    descr("Unassigns curation tasks.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(unassign)

  footer("")
}
