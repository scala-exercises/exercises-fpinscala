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

package fpinscalalib.customlib.errorhandling

case class Employee(name: String, department: String, manager: Option[String])

object ExampleHelper {
  val joe   = Employee("Joe", "Finances", Some("Julie"))
  val mary  = Employee("Mary", "IT", None)
  val izumi = Employee("Izumi", "IT", Some("Jaime"))

  def lookupByName(name: String): Option[Employee] =
    name match {
      case "Joe"   => Some(joe)
      case "Mary"  => Some(mary)
      case "Izumi" => Some(izumi)
      case _       => None
    }

  def lookupByNameViaEither(name: String): Either[String, Employee] =
    name match {
      case "Joe"   => Right(joe)
      case "Mary"  => Right(mary)
      case "Izumi" => Right(izumi)
      case _       => Left("Employee not found")
    }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def TryEither[A](a: => A): Either[String, A] =
    try Right(a)
    catch { case e: Exception => Left(e.getMessage) }
}
