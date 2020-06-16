/*
 * Copyright 2016-2020 47 Degrees Open Source <https://www.47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fpinscalalib

import org.scalaexercises.definitions._

/** Exercises based on Manning's "Functional Programming in Scala" book by Paul Chiusano and Rúnar Bjarnason.
 *
 * @param name fp_in_scala
 */
object FPinScalaLibrary extends Library {
  override def owner      = "scala-exercises"
  override def repository = "exercises-fpinscala"

  override def color = Some("#E22D34")

  override def sections =
    scala.collection.immutable.List(
      GettingStartedWithFPSection,
      FunctionalDataStructuresSection,
      ErrorHandlingSection,
      StrictnessAndLazinessSection,
      FunctionalStateSection,
      FunctionalParallelismSection,
      PropertyBasedTestingSection,
      ParserCombinatorsSection
    )

  override def logoPath = "fp_in_scala"
}
