
object Lambda {
  sealed trait Exp[T]
  case class Var[T](name: T)              extends Exp[T]
  case class Lam[T](arg: T, body: Exp[T]) extends Exp[T]
  case class App[T](f: Exp[T], x: Exp[T]) extends Exp[T]

  object Parse {
    import fastparse._
    import ScalaWhitespace.whitespace

    def ident[_ : P]: P[String] = P(CharsWhileIn("0-9a-zA-Z'").!)

    def varbl[_ : P]: P[Var[String]] = P(ident) map Var.apply

    def atom[_ : P]: P[Exp[String]] = P(varbl | ("(" ~ exp ~ ")"))

    def exp[_ : P]: P[Exp[String]] = P((atom ~ &(End | ")")) | abs | app)

    def abs[_ : P]: P[Lam[String]] = P(("\\" | "^" | "λ" | "lambda") ~ ident ~ ("." | ": ") ~ exp) map { case (x, b) => Lam(x, b) }

    def app[_ : P]: P[App[String]] = P(atom.rep(2)) map { case l::r::ls => ls.foldLeft(App(l,r))(App.apply) }

    def apply(s: String) = parse(s, exp(_)).get.value
  }
}
