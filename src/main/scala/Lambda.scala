
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

  def notAppEnd[_ : P]: P[Exp[S]] = P((varP ~ end) | (par ~ end) | (abs ~ end))
  def notApp[_ : P]: P[Exp[S]] = P(varP | par | abs)

  def exp[_ : P]: P[Exp[S]] = P(notAppEnd | app)

  def par[_ : P]: P[Exp[S]] = P("(" ~ exp ~ ")")

  def abs[_ : P]: P[Abs[S]] = P("\\" ~ ident ~ "." ~ exp) map tupled(Abs[S])

  def app[_ : P]: P[App[S]] = P(notApp.rep(2)) map {
    case l::r::ls => ls.foldLeft(App(l,r))(App.apply)
  }

  def varP[_ : P]: P[Var[S]] = P(ident) map Var[S]

  def end[_ : P]: P[Unit] = P(&(End ~ ")"))

  def apply(s: String) = parse(s, exp(_)).get.value

  def test[_ : P] = P(par ~ end)

  object Test {
    var id = -1
    def get_id = { id = id + 1 ; id }

    def apply(p: Parsed[Exp[S]]): Unit = p match {
      case Parsed.Success(value, index) => println(s"[$get_id] = $value")
      case failure: Parsed.Failure =>
        println(s"[$get_id] = ${failure.trace().longAggregateMsg}")
    }

    def apply(p: Parsed[Exp[S]], ps: Parsed[Exp[S]]*): Unit = { apply(p) ; ps foreach apply }
  }

  Test(
    parse("""(\mul.\two.mul two two) (\m.\n.\f.m(n f)) (\f.\x.f (f x))""", test(_)),
  )
}