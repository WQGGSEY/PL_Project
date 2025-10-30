package cs320

import Value._

object Implementation extends Template {

  trait Handler
  case class EmptyHandler() extends Handler
  case class FilledHandler(expression: Expr, env: Env, cont: Cont, handler: Handler) extends Handler

  def interp(expr: Expr): Value = interp(expr, Map(), (v: Value) => v, EmptyHandler())

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

  private def interp(expr: Expr, env: Env, cont: Cont, handler: Handler): Value = expr match{
    case Id(name: String) => cont(env.getOrElse(name, error("Runtime error")))
    case IntE(n: BigInt) => cont(IntV(n))
    case BooleanE(b: Boolean) => cont(BooleanV(b))
    case Add(l: Expr, r: Expr) => interp(l, env, lv => interp(r, env, rv => cont(val_add(lv, rv)), handler), handler)
    case Mul(l: Expr, r: Expr) => interp(l, env, lv => interp(r, env, rv => cont(val_mul(lv, rv)), handler), handler)
    case Div(l: Expr, r: Expr) => interp(l, env, lv => interp(r, env, rv => cont(val_div(lv, rv)), handler), handler)
    case Mod(l: Expr, r: Epxr) => interp(l, env, lv => interp(r, env, rv => cont(val_mod(lv, rv)), handler), handler)
    case Eq(l: Expr, r: Expr) => interp(l, env, lv => interp(r, env, rv => cont(val_eq(lv, rv)), handler), handler)
    case Lt(l: Expr, r: Expr) => interp(l, env, lv => interp(r, env, rv => cont(val_lt(lv, rv)), handler), handler)
    case If(cond: Expr, tBranch: Expr, fBranch: Expr) => interp(cond, env, cv => cv match{
      case BooleanV(true) => interp(tBranch, env, cont, handler)
      case BooleanV(false) => interp(fBranch, env, cont, handler)
      case _ => error("Runtime error")
    })
    case TupleE(exprs: List[Expr]) => exprs match{
      case Nil => TupleV(Nil)
      case h :: t => interp(h, env, hv => interp(TupleE(t), env, tv => cont(TupleV(hv :: tv.asInstanceOf[TupleV].values))), handler)
      case _ => error("Runtime error")
    }
    case Proj(expression: Expr, index: Int) => interp(expression, env, ev => ev match {
      case TupleV(values: List[Value]) => if(index < 1 || index > values.length) error("Runtime error") else cont(values(index - 1))
      case _ => error("Runtime error")
    }, handler)
    case NilE => cont(NilV)
    case ConsE(head: Expr, tail: Expr) => interp(head, env, hv => interp(tail, env, tv => tv match{
      case NilV => cont(ConsV(hv, NilV))
      case ConsV(_, _) => cont(ConsV(hv, tv))
      case _ => error("Runtime error")
    }, handler), handler)
    case Empty(expression: Expr) => interp(expression, env, ev => ev match{
      case NilV => cont(BooleanV(true))
      case ConsV(_, _) => cont(BooleanV(false))
      case _ => error("Runtime error")
    }, handler)
    case Head(expression: Expr) => interp(expression, env, ev => ev match{
      case ConsV(h, _) => cont(h)
      case _ => error("Runtime error")
    }, handler)
    case Tail(expression: Expr) => interp(expression, env, ev => ev match{
      case ConsV(_, t) => cont(t)
      case _ => error("Runtime error")
    }, handler)
    case Val(name: String, expression: Expr, body: Expr) => interp(expression, env, ev=> interp(body, env + (name -> ev), cont, handler), handler)
    case Vcc(name: String, body: Expr) => interp(body, env + (name -> ContV(cont)), cont, handler)
    case Fun(parameters: List[String], body: Expr) => cont(CloV(parameters, body, env), handler)
    case RecFuns(functions: List[FunDef], body: Expr) => {
      val temp_closures = functions.map(f => CloV(f.parameters, f.body, Map()))
      val mappings = functions.zip(temp_closures).map{case (f, clov) => (f.name, clov)}
      val new_env = env ++ mappings
      temp_closures.foreach{clov => clov.env = new_env}
      interp(body, new_env, cont, handler)
    }
    case App(function: Expr, arguments: List[Expr]) => interp(function, env, fv => {
      arguments match{
        // make recursive continuation calls work for each argument
        // base case
        case Nil => fv match{
          case CloV(parameters, body, fenv) => {
            if(parameters.length != 0) error("Runtime error")
            interp(body, fenv, cont)
          }
          case ContV(continuation) => continuation(NilV)
          case _ => error("Runtime error")
        }
        // recursive case
        case h :: t => interp(h, env, hv => interp(App(function, t), env, tv => fv match{
          case CloV(parameters, body, fenv) => {
            if(parameters.length != arguments.length) error("Runtime error")
            interp(body, fenv ++ parameters.zip(hv :: (tv match{
              case NilV => Nil
              case ConsV(head, tail) => head :: (tail match{
                case NilV => Nil
                case _ => error("Runtime error")
              })
              case _ => error("Runtime error")
            })), cont)
          }
          case ContV(continuation) => continuation(hv)
          case _ => error("Runtime error")
        }))
      }
    })
    case Test(expression: Expr, typ: Type) => interp(expression, env, ev => (ev, typ) match{
      case (IntV(_), IntT) => cont(BooleanV(true))
      case (BooleanV(_), BooleanT) => cont(BooleanV(true))
      case (TupleV(_), TupleT) => cont(BooleanV(true))
      case (NilV, ListT) => cont(BooleanV(true))
      case (ConsV(_, _), ListT) => cont(BooleanV(true))
      case (CloV(_, _, _), FunctionT) => cont(BooleanV(true))
      case (ContV(_), FunctionT) => cont(BooleanV(true))
      case _ => cont(BooleanV(false))
    }, handler)
    case Throw(expression: Expr) => interp(expression, env, ev => {
      handler match{
        case EmptyHandler() => error("Runtime error")
        case FilledHandler(hexpr, henv, hcont, hhandler) => interp(hexpr, henv, hv => {
          hv match{
            case CloV(parameters: List[String], body: Expr, fenv: Env) => {
              if(parameters.length != 1) error("Runtime error")
              interp(body, fenv + (parameters[0] -> ev), hcont, hhandler)
            }
            case ContV(continuation: Cont) => continuation(ev) 
            case _ => error("Runtime error")
          }

        }, hhandler)
      }
    })
    case Try(expression: Expr, handling_expr: Expr) => {
      val new_handler = FilledHandler(handling_expr, env, cont, handler)
      interp(expression, env, cont, new_handler)
    }
  }



}
