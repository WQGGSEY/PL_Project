package cs320

import Value._

object Implementation extends Template {

  def interp(expr: Expr): Value = sub_interp(expr, Map())


  def sub_interp(expr: Expr, env: Env): Value = expr match{
    case Num(n) => NumV(n)
    case Add(l, r) => (sub_interp(l, env), sub_interp(r, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _ => error("Not Numbers")
    }
    case Sub(l, r) => (sub_interp(l, env), sub_interp(r, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
      case _ => error("Not Numbers")
    }
    case Val(name, value, body) => sub_interp(body, env + (name -> sub_interp(value, env)))
    case Id(name) => env.getOrElse(name, error(s"Unbound identifier: $name"))
    case App(func, args) => {
      val val_args = args.map(e => sub_interp(e, env))
      sub_interp(func, env) match{
        case CloV(params, body, fenv) => {
          if(params.length != val_args.length) error("wrong arity")
          val new_env = params.zip(val_args).toMap ++ fenv
          sub_interp(body, new_env)
        }
        case _ => error("not a closure")
      }
    }
    case Fun(params, body) => CloV(params, body, env)
    case Rec(rec) => RecV(rec.map{ case (k, v) => (k, sub_interp(v, env)) })
    case Acc(expr, name) => sub_interp(expr, env) match {
      case RecV(records) => records.getOrElse(name, error("no such field"))
      case _ => error("not a record")
    }
  }
}
