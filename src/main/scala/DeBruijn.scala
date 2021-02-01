import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.language.{existentials => ftr}

object DeBruijn {

  trait Exp
  case class Var(depth: Int) extends Exp
  case class Abs(body: Exp) extends Exp
  case class App(f: Exp, x: Exp) extends Exp

  def from[T](exp: Lambda.Exp[T]) = {
    def rec(e: Lambda.Exp[T], getDepth: Map[T, Int] = Map.empty, depth: Int = 0): Exp = exp match {
      case Lambda.Var(name)      => Var(depth - getDepth(name))
      case Lambda.Abs(arg, body) => Abs(rec(body, getDepth.updated(arg, depth), depth + 1))
      case Lambda.App(f, x)      => App(rec(f, getDepth, depth), rec(x, getDepth, depth))
    }

    rec(exp)
  }

  private case class Arg[T](exp: Lambda.Exp[T], getDepth: Map[T, Int], depth: Int)
  private class From3[T] extends Recursion[Arg[T], Exp, Unit] {

    override protected def dispatcher(arg: Arg[T]): Either[Exp, (Unit, List[Arg[T]])] = {
      val Arg(exp, map, depth) = arg
      exp match {
        case Lambda.Var(name)      => Left(Var(map(name) - depth))
        case Lambda.Abs(arg, body) => Right(() -> List(Arg(body, map.updated(arg, depth), depth + 1)))
        case Lambda.App(f, x)      => Right(() -> List(Arg(f, map, depth), Arg(x, map, depth)))
      }
    }
    def combinator(op: Unit, ls: List[Exp]): Exp = ls match {
      case List(body) => Abs(body)
      case List(f, x) => App(f, x)
    }
  }
  def from3[T](exp: Lambda.Exp[T]) = new From3[T].get(Arg(exp, Map.empty, 0))
}



object Main extends App {
  println(
    fastparse.parse("""(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))""", Parse.exp(_))
  )
}
