import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.language.{existentials => ftr}

object DeBruijn {

  trait Exp
  case class Var(depth: Int) extends Exp {
    override def toString: String = depth.toString
  }

  case class Lam(body: Exp) extends Exp {
    override def toString: String = s"(\\.$body)"
  }

  case class App(f: Exp, x: Exp) extends Exp {
    override def toString: String = s"($f $x)"
  }

  def from[T](exp: Lambda.Exp[T]) = {
    def rec(e: Lambda.Exp[T], getDepth: Map[T, Int] = Map.empty, depth: Int = 0): Exp = e match {
      case Lambda.Var(name)      => Var(depth - getDepth(name))
      case Lambda.Lam(arg, body) => Lam(rec(body, getDepth.updated(arg, depth), depth + 1))
      case Lambda.App(f, x)      => App(rec(f, getDepth, depth), rec(x, getDepth, depth))
    }

    rec(exp)
  }

  private case class Arg[T](exp: Lambda.Exp[T], getDepth: Map[T, Int], depth: Int)
  private class From3[T] extends Recursion[Arg[T], Exp, Unit] {

    override protected def dispatcher(arg: Arg[T]): Either[Exp, (Unit, List[Arg[T]])] = {
      val Arg(exp, map, depth) = arg
      exp match {
        case Lambda.Var(name)      => Left(Var(depth - map(name)))
        case Lambda.Lam(arg, body) => Right(() -> List(Arg(body, map.updated(arg, depth), depth + 1)))
        case Lambda.App(f, x)      => Right(() -> List(Arg(f, map, depth), Arg(x, map, depth)))
      }
    }
    def combinator(op: Unit, ls: List[Exp]): Exp = ls match {
      case List(body) => Lam(body)
      case List(f, x) => App(f, x)
    }
  }
  def from3[T](exp: Lambda.Exp[T]) = new From3[T].get(Arg(exp, Map.empty, 0))
}



object Main extends App {
  val expr1 = fastparse.parse(
    LazyList(
      """(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))""",
      """(\m.\n.\f.m(n f))""",
    ).head,
    Lambda.Parse.exp(_)
  ).get.value

  println(expr1)
  println(DeBruijn.from(expr1))
  println(DeBruijn.from3(expr1))
}
