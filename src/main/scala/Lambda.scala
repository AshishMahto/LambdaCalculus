
object Lambda {
  sealed trait Exp[T] {
    def freeVars: Set[T]
  }

  case class Var[T](name: T) extends Exp[T] {
    override def freeVars = Set(name)
  }

  case class Abs[T](arg: T, body: Exp[T]) extends Exp[T] {
    override def freeVars = body.freeVars - arg
  }

  case class App[T](f: Exp[T], x: Exp[T]) extends Exp[T] {
    override def freeVars = f.freeVars | x.freeVars
  }
}

object Parse extends App {
  type S = String
  import Lambda._
  import fastparse._
  import ScalaWhitespace.whitespace
  import scala.Function.tupled

  def ident[_ : P]: P[String] = P(CharsWhileIn("0-9a-zA-Z").!)

  def variable[_ : P]: P[Var[S]] = P(ident) map Var[S]
  def varEnd[_ : P]: P[Var[S]] = P(ident ~ &(End | ")")) map Var[S]

  def paren[_ : P]: P[Exp[S]] = P("(" ~ exp ~ ")")

  def atom[_ : P]: P[Exp[S]] = P(variable | paren)

  def abs[_ : P]: P[Abs[S]] = P("\\" ~ ident ~ "." ~ exp) map tupled(Abs[S])

  def app[_ : P]: P[App[S]] = P(atom.rep(2)) map { case l::r::ls => ls.foldLeft(App(l,r))(App[S]) }

  def exp[_ : P]: P[Exp[S]] = P(varEnd | paren | abs | app)

  def apply(s: String) = parse(s, exp(_)).get.value


  object Test {
    var id = -1
    def get_id = { id = id + 1 ; id }

    def apply(ps: Parsed[Any]*): Unit = ps foreach {
      case Parsed.Success(value, _) => println(s"[$get_id] = $value")
      case failure: Parsed.Failure =>
        println(s"[$get_id] = ${failure.trace().longAggregateMsg}")
    }
  }


  def test[_ : P] = P(atom ~ atom ~ atom)

  Test(
    parse("""f x x""", test(_)),
    parse("""(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))""", paren(_)),
    parse("""(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))""", app(_)),
  )
}
