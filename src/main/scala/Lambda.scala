object Lambda {
  sealed trait Exp[T] { def freeVars: Set[T] }
  protected class FreeVars[T](val freeVars: Set[T]) extends Exp[T]

  case class Var[T](name: T)              extends FreeVars(Set(name))
  case class Lam[T](arg: T, body: Exp[T]) extends FreeVars(body.freeVars - arg)
  case class App[T](f: Exp[T], x: Exp[T]) extends FreeVars(f.freeVars | x.freeVars)

  object Parse {
    import fastparse._
    import ScalaWhitespace.whitespace

    def ident[_ : P]: P[String] = P(CharsWhileIn("0-9a-zA-Z").!)

    def variable[_ : P]: P[Var[String]] = P(ident) map Var.apply

    def atom[_ : P]: P[Exp[String]] = P(variable | ("(" ~ exp ~ ")"))

    def exp[_ : P]: P[Exp[String]] = P((atom ~ &(End | ")")) | abs | app)

    def abs[_ : P]: P[Lam[String]] = P("\\" ~ ident ~ "." ~ exp) map { case (x, b) => Lam(x, b) }

    def app[_ : P]: P[App[String]] = P(atom.rep(2)) map { case l::r::ls => ls.foldLeft(App(l,r))(App.apply) }

    def apply(s: String) = parse(s, exp(_)).get.value
  }
}
