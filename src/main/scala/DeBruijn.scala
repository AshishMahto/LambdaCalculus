import scala.language.{existentials => ftr, implicitConversions}

object DeBruijn {

  sealed trait Exp {
    import Eval.Reduction
    def aoe(reps: Int = 1): Reduction = Eval.aoe(this, reps)
    def nor(reps: Int = 1): Reduction = Eval.nor(this, reps)
    def eta(reps: Int = 1): Reduction = Eval.eta(this, reps)
    def subst(sub: Exp): Exp = Eval.subst(this, sub)
    protected[DeBruijn] def free(depth: Int): Set[Int]
    def freeVars = free(1)
  }

  case class Free(name: String) extends Exp {
    def free(depth: Int) = Set()
    override def toString: String = name
  }

  case class Var(id: Int) extends Exp {
    def free(depth: Int) = Set(depth - id + 1)
    override def toString: String = id.toString
  }

  case class Lam(body: Exp) extends Exp {
    def free(depth: Int) = body.free(depth+1) - depth
    override def toString: String = s"(\\.$body)"
  }

  case class App(f: Exp, x: Exp) extends Exp {
    def free(depth: Int) = f.free(depth) | x.free(depth)
    override def toString: String = s"($f $x)"
  }

  def from[T](exp: Lambda.Exp[T]) = {
    def rec(e: Lambda.Exp[T], getDepth: Map[T, Int], depth: Int): Exp = e match {
      case Lambda.Var(name) => (getDepth get name fold[Exp] Free(name.toString)) { i => Var(depth - i) }
      case Lambda.Lam(x, m) => Lam(rec(m, getDepth.updated(x, depth), depth + 1))
      case Lambda.App(f, x) => App(rec(f, getDepth, depth), rec(x, getDepth, depth))
    }
    rec(exp, Map(), 0)
  }


  object Eval {
    case class Reduction(reduced: Exp, howMany: Int = 0) {
      def addReps(i: Int) = this.copy(howMany = howMany + i)
      def wrap(f: Exp => Exp): Reduction = this.copy(reduced = f(reduced))
    }

    def rename(sub: Int, exp: Exp): Exp = {
      def rec(exp: Exp, depth: Int): Exp = exp match {
        case Free(_)   => exp
        case Var(id)   => if (id < depth) Var(id) else Var(id + sub - 1)
        case Lam(body) => Lam(rec(body, depth + 1))
        case App(f, x) => App(rec(f, depth), rec(x, depth))
      }
      rec(exp, 1)
    }
    def subst(exp: Exp, sub: Exp): Exp = {
      def rec(exp: Exp, depth: Int): Exp = exp match {
        case Free(_)   => exp
        case Var(id)   =>
               if (id < depth) Var(id)
          else if (id > depth) Var(id - 1)
          else rename(depth, sub)
        case Lam(body) => Lam(rec(body, depth + 1))
        case App(f, x) => App(rec(f, depth), rec(x, depth))
      }
      rec(exp, 1)
    }

    def aoe(exp: Exp, reps: Int): Reduction = exp match {
      case App(f, x) if reps > 0 =>
        val Reduction(f_red, some) = f aoe reps
        val Reduction(x_red, used) = x aoe (reps - some)

        (f_red, reps - used) match {
          case (Lam(b), r) if r > 0 => b subst x_red aoe r - 1 addReps used + 1
          case _                    => Reduction(App(f_red, x_red), used)
        }

      case _ => Reduction(exp)
    }

    def eta(exp: Exp, reps: Int): Reduction = exp match {
      case App(f, x) =>
        val Reduction(f_red, some) = f eta reps
        val Reduction(x_red, used) = x eta (reps - some)
        Reduction(App(f_red, x_red), used)
      case Lam(body) => body eta reps match {
        case Reduction(App(f, Var(1)), used)
          if used < reps && !exp.freeVars(1) => Reduction(f, used + 1)
        case x                               => x
      }
      case _ => Reduction(exp)
    }

    def norOne(exp: Exp): Reduction = exp match {
      case App(Lam(b), x) => Reduction(b subst x, 1)
      case App(f, x) =>
        val Reduction(f_red, used1) = norOne(f)
        val Reduction(x_red, used2) = if (used1 == 0) norOne(x) else Reduction(x)
        Reduction(App(f_red, x_red), used1 + used2)

      case Lam(body) => norOne(body) wrap Lam
      case _         => Reduction(exp)
    }

    def nor(exp: Exp, reps: Int): Reduction = {
      var cur = Reduction(exp)
      1 to reps foreach { _ =>
        val Reduction(nxt, done) = norOne(cur.reduced)
        if (done == 0) return cur
        cur = Reduction(nxt, cur.howMany + 1)
      }
      cur
    }
  }
}
