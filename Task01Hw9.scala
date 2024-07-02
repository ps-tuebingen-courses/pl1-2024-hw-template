import scala.language.higherKinds
import scala.language.implicitConversions

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
  // The "monad laws":
  // 1) "unit" acts as a kind of neutral element of "bind", that is:
  //    1a) bind(unit(x),f) == f(x) and
  //    1b) bind(x, y => unit(y)) == x
  // 2) Bind enjoys an associative property
  //     bind(bind(x,f),g) == bind(x, y => bind(f(y),g))
}

extension [A, M[_]](m: M[A])(using mm: Monad[M])
  def map[B](f: A => B): M[B] = mm.bind(m, (x: A) => mm.unit(f(x)))
  def flatMap[B](f: A => M[B]): M[B] = mm.bind(m, f)

object OptionMonad extends Monad[Option] {
  override def bind[A,B](a: Option[A], f: A => Option[B]): Option[B] =
    a match {
      case Some(x) => f(x)
      case None => None
    }
  override def unit[A](a: A) = Some(a)
}
case class Person(id: Int, firstName: String, lastName: String)
case class Department(id: Int, head: Person, members: Map[Int, Person])
case class Company(departments: Map[Int, Department])

def findEmployeeLastName(company: Company, depId: Int, personId: Int): Option[String] =
  for {
    dep <-company.departments.get(depId)
    person <- dep.members.get(personId)
  } yield person.lastName


def findEmployeeSuperior(company: Company, depId: Int, personId: Int): Option[Person] =
  for {
    dep <-company.departments.get(depId)
    person <- dep.members.get(personId)
  }yield dep.head
val company = Company(Map(
  1 -> Department(1, Person(1, "John", "Doe"), Map(1 -> Person(1, "Alice", "Smith")))
))
println(findEmployeeLastName(company, 1, 1))
println(findEmployeeLastName(company, 2, 1))

println(findEmployeeSuperior(company, 1, 1))  
println(findEmployeeSuperior(company, 2, 1))  
/*Task 1.2
def findEmployeeSuperior(company: Company, depId: Int, personId: Int): Option[Person] =
  for {
    dep <- company.departments.get(depId)
  } yield dep.head
Problem with this code is that it does not check if person from personId works in that department, instead it just returns the head of the department.
*/