
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

    while (argStack.nonEmpty) {
      val fst :: rest = argStack
      argStack = rest
      dispatcher(fst) match {
        case Left(ret) => retStack ::= ret
        case Right((op, args)) =>
          argStack = args reverse_::: argStack
          opsStack ::= op -> args.length
      }
    }

    while (opsStack.nonEmpty) {
      val (op, i) :: opRest = opsStack
      opsStack = opRest
      val (args, retRest) = retStack.splitAt(i)
      retStack = combinator(op, args) :: retRest
    }

    retStack.head
  }
}
