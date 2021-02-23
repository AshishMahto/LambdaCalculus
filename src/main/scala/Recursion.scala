
/** A class for flattening some recursive functions to iteration with stacks. */
trait Recursion[Arg, Ret, Symbol] {
  /** Handle a base case, or a recursive case, exposing all recursive calls required to complete it.
   * @return Ret - a return value for a base case. <br>
   *      Symbol - a "name" for this recursive case, to be used later <br>
   *   List[Arg] - a list of recursive calls required to complete this base case */
  protected def dispatcher(arg: Arg): Either[Ret, (Symbol, List[Arg])]

  /** Handle the rest of the recursive case, combining the recursive results into a value.
   * @param op The symbol saved from earlier.
   * @param ls An equivalent list of recursive results from earlier. */
  protected def combinator(op: Symbol, ls: List[Ret]): Ret

  /** Run the recursion with an initial argument. */
  def get(init: Arg): Ret = {
    var retStack = List[Ret]()
    var argStack = List[Arg](init)
    var opsStack = List[(Symbol, Int)]()

    def popArg = { val arg :: rest = argStack ; argStack = rest ; arg }
    def popRet = { val ret :: rest = retStack ; retStack = rest ; ret }

    while (argStack.nonEmpty) {
      dispatcher(popArg) match {
        case Left(ret) =>
          retStack ::= ret
          opsStack ::= null.asInstanceOf[Symbol] -> 0
        case Right((op, args)) =>
          argStack = args reverse_::: argStack
          opsStack ::= op -> args.length
      }
    }

    var recStack = List[Ret]()
    def popRecs(i: Int) = {
      var ls = List[Ret]()
      1 to i foreach { _ => ls ::= recStack.head ; recStack = recStack.tail }
      ls
    }

    opsStack foreach {
      case (_, 0) => recStack ::= popRet
      case (op, i) => recStack = combinator(op, popRecs(i)) :: recStack
    }

    recStack.head
  }
}


/** A version for recursive functions that don't require a symbol.
 * (i.e, recursive cases have distinct numbers of recursive calls */
trait Recursion_NoSymbol[Arg, Ret] { self =>
  /** Handle a base case, or a recursive case, exposing all recursive calls required to complete it.
   * @return Ret - a return value for a base case. <br>
   *      Symbol - a "name" for this recursive case, to be used later <br>
   *   List[Arg] - a list of recursive calls required to complete this base case */
  protected def dispatcher(arg: Arg): Either[Ret, List[Arg]]

  /** Handle the rest of the recursive case, combining the recursive results into a value.
   * @param ls An equivalent list of recursive results from earlier. */
  protected def combinator(ls: List[Ret]): Ret

  private val inner = new Recursion[Arg, Ret, Unit] {
    def combinator(op: Unit, ls: List[Ret]): Ret = self.combinator(ls)

    def dispatcher(arg: Arg): Either[Ret, (Unit, List[Arg])] = self.dispatcher(arg).map(() -> _)
  }

  /** Run the recursion with an initial argument. */
  def get(init: Arg): Ret = inner.get(init)
}
