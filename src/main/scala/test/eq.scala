package test

import shapeless._


/*
  We want to automatically generate instances of a particular typeclass using the shapeless library.
  The typeclass we are interested in is the Eq typeclass which determines whether two given instances of a type are equal

  Part 1)
  Implement implicit Eq instances for the scala primitives Boolean, Double, Int, and String.

  Part 2)
  Given Eq instances of all types of an HList, generate an Eq instance for that HList.
  Note, the function signatures are incomplete. Additionally, you may need additional functions to make this work

  Part 3)
  Given Eq instances of all types of a Coproduct, generate an Eq instance for that Coproduct

  Part 4)
  Generate Eq instances of all ADTs (represented as case classes and sealed traits)

  Your code should compile and successfully run the code found in EqTest
  You may wish to add additional tests
 */

sealed trait Eq[T] {
  def eqv(t1: T, t2: T): Boolean
  def neqv(t1: T, t2: T): Boolean = !eqv(t1, t2)
}

object Eq {
  def apply[T](implicit ev: Eq[T]) = ev
  def instance[T](f: (T, T) => Boolean): Eq[T] = new Eq[T] {
    def eqv(t1: T, t2: T): Boolean = f(t1, t2)
  }

  // Part 1
  implicit val intEq:     Eq[Int]     = Eq.instance((t1,t2) => t1 == t2)
  implicit val doubleEq:  Eq[Double]  = Eq.instance((t1,t2) => t1 == t2)
  implicit val booleanEq: Eq[Boolean] = Eq.instance((t1,t2) => t1 == t2)
  implicit val stringEq:  Eq[String]  = Eq.instance((t1,t2) => t1.equals(t2))

  // Part 2
  
  implicit val hnilEq = Eq.instance[HNil]((x, y) => true)
  
  implicit def hconsEq[H, T <: HList](implicit hEq: Eq[H], tEq: Eq[T]): Eq[H :: T] =
    Eq.instance[H :: T]((l1, l2) =>  hEq.eqv(l1.head, l2.head) && tEq.eqv(l1.tail, l2.tail))

  // Part 3
  implicit def cnilEq = Eq.instance[CNil]((x,y) => throw new RuntimeException("CNil should never be reached"))
    
  implicit def cconsEq[L, R <: Coproduct](implicit evL: Eq[L], evR: Eq[R]): Eq[L :+: R] = 
    Eq.instance[L :+: R]((l1,l2) => 
      (l1, l2) match { 
        case (Inl(h1), Inl(h2)) => evL.eqv(h1, h2)  
        case (Inr(t1), Inr(t2)) => evR.eqv(t1, t2) 
        case _ => false
      }
    )

  // Part 4
  implicit def genericEq[T, Repr]
  (implicit tgen: Generic.Aux[T, Repr], reprEq: Eq[Repr]): Eq[T] = 
    Eq.instance[T]((t1, t2) => reprEq.eqv(tgen.to(t1), tgen.to(t2)))
    

}

trait EqSyntax[T] {
  def ===(b: T): Boolean
  def !==(b: T): Boolean
}

object EqSyntax {
  implicit def eqSyntax[T](a: T)(implicit ev: Eq[T]): EqSyntax[T] = new EqSyntax[T] {
    def ===(b: T): Boolean = ev.eqv(a, b)
    def !==(b: T): Boolean = ev.neqv(a, b)
  }
}

object EqTest extends App {
  type Product = Int :: String :: Boolean :: Double :: HNil
  type CoProduct = Int :+: String :+: Boolean :+: Double :+: CNil

  val productEq   = Eq[Product]
  val coProductEq = Eq[CoProduct]

  import EqSyntax._

  val product1 = 1 :: "abc" :: false :: 0.0 :: HNil
  val product2 = 2 :: "def" :: true  :: 1.0 :: HNil

  assert(product1 === product1)
  assert(product2 === product2)
  assert(product1 !== product2)

  val coproduct1 = Coproduct[CoProduct](3.0)
  val coproduct2 = Coproduct[CoProduct](false)

  assert(coproduct1 === coproduct1)
  assert(coproduct2 === coproduct2)
  assert(coproduct1 !== coproduct2)

  sealed trait FooBar
  case class Foo(i: Int, s: String) extends FooBar
  case class Bar(b: Boolean, d: Double) extends FooBar

  val fooEq    = Eq[Foo]
  val barEq    = Eq[Bar]
  val fooBarEq = Eq[FooBar]

  val foo1 = Foo(1, "abc")
  val foo2 = Foo(2, "def")

  assert(foo1 === foo1)
  assert(foo1 === foo1)
  assert(foo1 !== foo2)

  val bar1 = Bar(false, 0.0)
  val bar2 = Bar(true, 1.0)
  assert(bar1 === bar1)
  assert(bar1 === bar1)
  assert(bar1 !== bar2)

  val fooBar1: FooBar = foo1
  val fooBar2: FooBar = bar1

  assert(fooBar1 === fooBar1)
  assert(fooBar2 === fooBar2)
  assert(fooBar1 !== fooBar2)
}