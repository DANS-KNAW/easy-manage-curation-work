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

import better.files.File.currentWorkingDirectory
import org.apache.commons.configuration.PropertiesConfiguration
import org.scalatest.{ FlatSpec, Matchers }

import scala.collection.JavaConverters._

class DebugConfigSpec extends FlatSpec with Matchers {

  "debug-config" should "contain the same files as src/main/assembly/dist/cfg" in {
    val filesInDebugConfig = (currentWorkingDirectory / "src" / "test" / "resources" / "debug-config").list
    val filesInDistCfg = (currentWorkingDirectory / "src" / "main" / "assembly" / "dist" / "cfg").list

    filesInDebugConfig.map(_.name).toSet shouldBe filesInDistCfg.map(_.name).toSet
  }

  it should "contain an application.properties with the same keys as the one in src/main/assembly/dist/cfg" in {
    val propsInDebugConfig = new PropertiesConfiguration((currentWorkingDirectory / "src" / "test" / "resources" / "debug-config" / "application.properties").toJava)
    val propsInDistCfg = new PropertiesConfiguration((currentWorkingDirectory / "src" / "main" / "assembly" / "dist" / "cfg" / "application.properties").toJava)

    propsInDebugConfig.getKeys.asScala.toSet shouldBe propsInDistCfg.getKeys.asScala.toSet
  }
}
