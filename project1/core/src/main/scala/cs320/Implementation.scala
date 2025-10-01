package cs320

import Value._

object Implementation extends Template {

  def numOp(op: (BigInt, BigInt)=> BigInt): (Value, Value) => Value = (_, _) match{
	  case (IntV(x), IntV(y)) => IntV(op(x, y))
	  case (x, y) => error("Runtime error")
  }

  def compOp(op: (BigInt, BigInt) => Boolean): (Value, Value) => Value = (_, _) match{
    case (IntV(x), IntV(y)) => BooleanV(op(x, y))
    case (x, y) => error("Runtime error")
  }

  val val_add = numOp((x: BigInt, y: BigInt) => x + y)
  val val_mul = numOp((x: BigInt, y: BigInt) => x * y)
  val val_div = numOp((x: BigInt, y: BigInt) => if(y == 0) error("Runtime error") else x / y)
  val val_mod = numOp{(x: BigInt, y: BigInt) => if(y == 0) error("Runtime error") else x % y}
  val val_eq = compOp((x: BigInt, y: BigInt) => x == y) 
  val val_lt = compOp((x: BigInt, y: BigInt) => x < y)

  def interp(expr: Expr): Value = interp(expr, Map())

  def interp(expr: Expr, env: Env): Value = expr match{
    case Id(name) => env.getOrElse(name, error("Runtime error"))
    case IntE(value) => IntV(value)
    case BooleanE(value) => BooleanV(value)
    case Add(l, r) => val_add(interp(l, env), interp(r, env))
    case Mul(l, r) => val_mul(interp(l, env), interp(r, env))
    case Div(l, r) => val_div(interp(l, env), interp(r, env))
    case Mod(l, r) => val_mod(interp(l, env), interp(r, env))
    case Eq(l, r) => val_eq(interp(l, env), interp(r, env))
    case Lt(l, r) => val_lt(interp(l, env), interp(r, env))
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
