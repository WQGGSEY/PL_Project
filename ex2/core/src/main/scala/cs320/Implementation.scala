package cs320

object Implementation extends Template {

  def freeIds(expr: Expr): Set[String] = {
    val bound: Set[String] = Set();
    def free(expr: Expr, bound: Set[String]): Set[String] = expr match{
      case Num(_) => Set()
      case Add(left, right) => free(left, bound) ++ free(right, bound)
      case Sub(left, right) => free(left, bound) ++ free(right, bound)
      case Id(id) => if(bound.contains(id)) Set() else Set(id)
      case Val(name, expr, body) => free(expr, bound) ++ free(body, bound + name)
    }
    free(expr, bound)
  }

  def bindingIds(expr: Expr): Set[String] = {
    expr match{
      case Num(_) => Set()
      case Add(left, right) => bindingIds(left) ++ bindingIds(right)
      case Sub(left, right) => bindingIds(left) ++ bindingIds(right)
      case Id(_) => Set()
      case Val(name, expr, body) => Set(name) ++ bindingIds(expr) ++ bindingIds(body)
    }
  }

  def boundIds(expr: Expr): Set[String] = {
    val bind: Set[String] = Set();
    def bound(expr: Expr, bind: Set[String]): Set[String] = expr match{
      case Num(_) => Set()
      case Add(left, right) => bound(left, bind) ++ bound(right, bind)
      case Sub(left, right) => bound(left, bind) ++ bound(right, bind)
      case Id(id) => if(bind.contains(id)) Set(id) else Set()
      case Val(name, expr, body) => bound(expr, bind) ++ bound(body, bind + name)
    }
    bound(expr, bind)
  }
}