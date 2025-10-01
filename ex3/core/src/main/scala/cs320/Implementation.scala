package cs320

object Implementation extends Template {

  // apply a binary numeric function on all the combinations of numbers from
  // the two input lists, and return the list of all the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l, r)
      rs.map(f) ++ binOp(op, rest, rs)
  }


  type Env = Map[String, List[Int]]
  def interp(expr: Expr, env: Env): List[Int] = expr match{
    case Num(values) => values
    case Add(left, right) => binOp(_ + _, interp(left, env), interp(right, env))
    case Sub(left, right) => binOp(_ - _, interp(left, env), interp(right, env))
    case Val(name, expr, body) => {
      interp(body, env + (name -> interp(expr, env)) )
      }
    case Id(id) => env.getOrElse(id, error("free identifier"))
    case Min(left, mid, right) => binOp(_ min _, binOp(_ min _, interp(left, env), interp(mid, env)), interp(right, env))
    case Max(left, mid, right) => binOp(_ max _, binOp(_ max _, interp(left, env), interp(mid, env)), interp(right, env))
  }

  def interp(expr: Expr): List[Int] = interp(expr, Map())
}
