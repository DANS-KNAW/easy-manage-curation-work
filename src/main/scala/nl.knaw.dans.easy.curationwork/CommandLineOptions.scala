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

import org.rogach.scallop.{ScallopConf, ScallopOption, Subcommand, singleArgConverter}

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
       |  $printedName assign <easy-datamanager> <UUID>
       |  $printedName unassign [<easy-datamanager> [<UUID>]]
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
  //val url = opt[String]("someOption", noshort = true, descr = "Description of the option", default = app.someProperty)

  val list = new Subcommand("list") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", required = false)
    descr("Lists the current curation tasks.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(list)

  val assign = new Subcommand("assign") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", required = true)
    val uuid: ScallopOption[DatamanagerId] = trailArg("uuid", required = true)
    descr("Assigns curation task to a datamanager.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(assign)

  val unassign = new Subcommand("unassign") {
    val datamanager: ScallopOption[DatamanagerId] = trailArg("easy-datamanager", required = false)
    val uuid: ScallopOption[DatamanagerId] = trailArg("uuid", required = false)
    descr("Unassigns curation tasks.")
    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(unassign)

  footer("")
}
