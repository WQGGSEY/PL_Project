package cs320

import Value._

object Implementation extends Template {

  type Sto = Map[Addr, Value]
  def interp(expr: Expr): Value = interp(expr, Map(), Map())._1

  private def valueOp(op: (Int, Int) => Int): (Value, Value) => Value = {
    (_, _) match {
      case (NumV(n1), NumV(n2)) => NumV(op(n1, n2))
      case _ => error("not a number")
    }
  }

  private val numVadd = valueOp(_ + _)
  private val numVsub = valueOp(_ - _)

  private def interp(expr: Expr, env: Env, sto: Sto): (Value, Sto) = expr match{
    case Num(n) => (NumV(n), sto)

    case Add(l, r) => {
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVadd(lv, rv), rs)
    }

    case Sub(l, r) => {
      val (lv, ls) = interp(l, env, sto)
      val (rv, rs) = interp(r, env, ls)
      (numVsub(lv, rv), rs)
    }

    case Id(name) => {
      val v = env.getOrElse(name, error("free identifier"))
      (v, sto)
    }

    case Fun(param, body) => (CloV(param, body, env), sto)

    case App(fun, arg) => {
      val (fv, fs) = interp(fun, env, sto)
      fv match {
        case CloV(param, body, fenv) => {
          val (av, as) = interp(arg, env, fs)
          interp(body, fenv + (param -> av), as)
        }
        case _ => error("not a closure")
      }
    }

    case NewBox(expr) => {
      val (ev, es) = interp(expr, env, sto)
      val addr = malloc(es)
      (BoxV(addr), es + (addr -> ev))
    }

    case SetBox(box, expr) => {
      val (bv, bs) = interp(box, env, sto)
      bv match {
        case BoxV(addr) =>{
          val (ev, es) = interp(expr, env, bs)
          (ev, es + (addr -> ev))
        }
        case _ => error("not a box")
      }
    }

    case OpenBox(box) => {
      val (bv, bs) = interp(box, env, sto)
      bv match {
        case BoxV(addr) => {
          (sto.getOrElse(addr, error("unallocated address")), bs)
        }
        case _ => error("not a box")
      }
    }

    case Seqn(l, r) => {
      val init = interp(l, env, sto)
      r.foldLeft(init){
        case ((v, s), r) => interp(r, env, s)
      }
    }

    case Rec(fields) => {
      val (val_fields, s) = fields.foldLeft(Map[String, Addr](), sto){
        case ((map, msto), (name, expr)) => {
          val (ev, es) = interp(expr, env, msto)
          val addr = malloc(es)
          (map + (name -> addr), msto + (addr -> ev))
        }
      }
      (RecV(val_fields), s)
    }

    case Get(r, name) => {
      val (rv, rs) = interp(r, env, sto)
      rv match {
        case RecV(fields) => {
          val addr = fields.getOrElse(name, error("no such field"))
          (sto.getOrElse(addr, error("unallocated address")),rs)
        }
        case _ => error("not a record")
      }
    }

    case Set(record, name, expr) => {
      val (rv, rs) = interp(record, env, sto)
      rv match {
        case RecV(fields) => {
          val addr = fields.getOrElse(name, error("no such field"))
          val (ev, es) = interp(expr, env, rs)
          (ev, es + (addr->ev))
        }
        case _ => error("not a record")
      }
    }

  }
  private def malloc(sto: Sto): Addr = sto.keys.maxOption.map(_ + 1).getOrElse(0)
}
