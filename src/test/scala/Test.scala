
object Test extends App {

  import scala.collection.mutable
  import DeBruijn.{App, Free, Apps}

  def fromString(s: String) = DeBruijn.from(Lambda.Parse(s))

  val zero = "^f.^x. x"
  val one = "^f.^x. f x"
  val two = "^f.^x. f (f x)"

  List(zero, one, two).foreach(x => println(fromString(x)))

  def lambdaStrFromNat(n: Int) = {
    def rec(n: Int = n): String = n match {
      case 0 => "x"
      case _ if n > 0 => s"(f ${rec(n-1)})"
    }
    s"^f. ^x. ${rec()}"
  }

  def fromNatFn(n: Int) = {
    val s = lambdaStrFromNat(n)
    println(s"Loading $n = $s")
    fromString(s)
  }
  val fromNat = new mutable.Map.WithDefault(mutable.Map[Int, DeBruijn.Exp](), fromNatFn)

  val add = fromString(s"^n. ^m. ^f. ^x. ((n f) ((m f) x))")
  val addTest = Apps(add, fromNat(5), fromNat(7), Free("f"), Free("x"))
  println(addTest)
  println(addTest.aoe(30).reduced)

  val mul = fromString(s"^n.^m.^f. n (m f)")
  val mulTest = Apps(mul, fromNat(3), fromNat(4), Free("f"), Free("x"))
  println(mulTest)
  println(mulTest.aoe(30).reduced)
}
