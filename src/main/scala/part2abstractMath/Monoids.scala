package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]):T =
  //    list.foldLeft(/* WHAT? */)(_ |+| _)

  // MONOIDS

  import cats.Monoid

  val intMonoid = Monoid[Int]
  val combinedInt = intMonoid.combine(23, 999) //1022
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring the implicit Monoid[String] in scope

  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoids")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]

  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension methods for Monoids - |+|

  //  import cats.syntax.monoid._ // either this one or cats.syntax.semigroup._

  val combineOptionFancy = Option(3) |+| Option(7)

  // TODO: Implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  import cats.instances.map._

  // TODO: Combine a list of phone books as Map[String, Int]
  // hint: don't construct a monoid - use an import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  val massivePhonebook = combineFold(phonebooks)

  // TODO: Shopping cart and online stores with Monoids
  // hint: define your own monoid - Monoid.instance
  // hint: use combineByFold
  implicit val shoppingCartMonoid = Monoid.instance[ShoppingCart](ShoppingCart(List(), 0.0), (s1, s2) => {
    ShoppingCart(s1.items ++ s2.items, s1.total + s2.total)
  })

  case class ShoppingCart(items: List[String], total: Double)

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(List("hi ", "there ", "how")))
    println(combineFold(List(10, 20, 30)))

    println(massivePhonebook)

    val shoppingCarts = List(
      ShoppingCart(List("Book", "Pen", "Pencil"), 100.2),
      ShoppingCart(List("Car", "Monitor", "Cup"), 204.3)
    )

    println(checkout(shoppingCarts))
  }
}