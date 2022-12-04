package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None  // Eq[Some[Int]]

  // variance
  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant
  // variance affect how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow") // implementation not important

  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat] // ok - TC instance for Animal is also applicable to Cats
  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizedShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs always use the more specific TC instance for that type
  // but may confuse the compiler ig the general TC is also present

  // rule 3: you can't have both benefits
  // Cats uses INVARIANT TCs

  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizedShow[Cat]) // ok - the compiler will inject CatsShow as implicit
    //    println(organizedShow[Animal]) // will not compile
  }

}