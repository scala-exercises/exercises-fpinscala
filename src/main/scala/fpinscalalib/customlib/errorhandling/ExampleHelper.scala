package fpinscalalib.customlib.errorhandling

case class Employee(name: String, department: String, manager: Option[String])

object ExampleHelper {
  val joe = Employee("Joe", "Finances", Some("Julie"))
  val mary = Employee("Mary", "IT", None)
  val izumi = Employee("Izumi", "IT", Some("Jaime"))

  def lookupByName(name: String): Option[Employee] = name match {
    case "Joe" => Some(joe)
    case "Mary" => Some(mary)
    case "Izumi" => Some(izumi)
    case _ => None
  }

  def lookupByNameViaEither(name: String): Either[String, Employee] = name match {
    case "Joe" => Right(joe)
    case "Mary" => Right(mary)
    case "Izumi" => Right(izumi)
    case _ => Left("Employee not found")
  }
}