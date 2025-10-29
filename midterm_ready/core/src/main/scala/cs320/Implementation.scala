package cs320

import Value._

object Implementation extends Template {

  def strict(v: Value) => Value = v match{
    case ev@ExprV(expr, env cache) => cache match{
      case Some(value) => value
      case _ => {
        val value = strict(interp(expr, env))
        ev.v = Some(value)
        value
      }
    }
    case _ => v
  }


  def numOp[V](op: (BigInt, BigInt)=> V): (Value, Value) => V = (l, r) => (strict(l), strict(r)) match {
    case (IntV(x), IntV(y)) => op(x+y)
    case _ => error("Runtime error")
  }

  val addOp = numOp[IntV]((x, y) => IntV(x + y))
  val mulOp = numOp[IntV]((x, y) => IntV(x * y))
  val divOp = numOp[IntV]((x, y) => if(y == 0) error("Division by zero") else IntV(x / y))
  val modOp = numOp[IntV]((x, y) => if(y == 0) error("Division by zero") else IntV(x % y))
  val eqOp  = numOp[BooleanV]((x, y) => BooleanV(x == y))
  val ltOp  = numOp[BooleanV]((x, y) => BooleanV(x < y))

  def interp(expr: Expr): Value = interp(expr, Map())

  def interp(expr: Expr, env: Env): Value = expr match{
    case Id(name) => env.getOrElse(name, error("Runtime error"))
    case IntE(value) => IntV(value)
    case BooleanE(value) => BooleanV(value)
    case Add(l, r) => addOp(interp(l, env), interp(r, env))
    case Mul(l, r) => mulOp(interp(l, env), interp(r, env))
    case Div(l, r) => divOp(strict(interp(l, env)), strict(interp(r, env)))
    case Mod(l, r) => modOp(strict(interp(l, env)), strict(interp(r, env)))
    case Eq(l, r) => eqOp(interp(l, env), interp(r, env))
    case Lt(l, r) => ltOp(interp(l, env), interp(r, env))
    case If(condition, trueBranch, falseBranch) => interp(condition, env) match{
      case BooleanV(true) => interp(trueBranch, env)
      case BooleanV(false) => interp(falseBranch, env)
      case _ => error("Runtime error")
    }
    case TupleE(expressions) => TupleV(expressions.map(interp(_, env)))
    case Proj(expression, index) => interp(expression, env) match{
      case TupleV(values) => if(index < 1 || index > values.length) error("Runtime error") else values(index - 1)
      case _ => error("Runtime error")
    }
    case NilE => NilV
    case ConsE(head, tail) => interp(tail, env) match{
      case NilV => ConsV(interp(head, env), NilV)
      case ConsV(_, _) => ConsV(interp(head, env), interp(tail, env))
      case _ => error("Runtime error")
    }
    case Empty(expression) => interp(expression, env) match{
      case NilV => BooleanV(true)
      case ConsV(_, _) => BooleanV(false)
      case _ => error("Runtime error")
    }
    case Head(expression) => interp(expression, env) match{
      case ConsV(h, _) => h
      case _ => error("Runtime error")
    }
    case Tail(expression) => interp(expression, env) match{
      case ConsV(_, t) => t
      case _ => error("Runtime error")
    }
    case Val(name, expression, body) => interp(body, env + (name -> interp(expression, env)))
    case Fun(parameters, body) => CloV(parameters, body, env)
    // case class FunDef(name: String, parameters: List[String], body: Expr)
    case RecFuns(functions, body) => {
      val temp_closures = functions.map(f => CloV(f.parameters, f.body, Map()))
      val mappings = functions.zip(temp_closures).map{case (f, clov) => (f.name, clov)}
      val new_env = env ++ mappings
      temp_closures.foreach{clov => clov.env = new_env}
      interp(body, new_env)
    }
    case App(function, arguuments) => interp(function, env) match{
      case CloV(parameters, body, fenv) => {
        if(parameters.length != arguuments.length) error("Runtime error")
        interp(body, fenv ++ parameters.zip(arguuments.map(interp(_, env))))
      }
      case _ => error("Runtime error")
    }
    case App(function, arguments) => {
      val fv = interp(function, env)
      val avs = arguments.map(ExprV(_, env, None))
      fv match{
        case CloV(parameters, body, fenv) => {
          if(parameters.length != avs.length) error("Runtime error")
          interp(body, fenv ++ parameters.zip(avs))
        }
        case _ => error("Runtime error")
      }
    }

    case Test(expression, typ) => (interp(expression, env), typ) match{
      case (IntV(_), IntT) => BooleanV(true)
      case (BooleanV(_), BooleanT) => BooleanV(true)
      case (TupleV(_), TupleT) => BooleanV(true)
      case (NilV, ListT) => BooleanV(true)
      case (ConsV(_, _), ListT) => BooleanV(true)
      case (CloV(_, _, _), FunctionT) => BooleanV(true)
      case _ => BooleanV(false)
    }
    case _ => error("Runtime error")
  }
}
