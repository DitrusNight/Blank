package blank

import blank.LifetimeSolving.addConstraint

object LifetimeSolving {

  //                                L1 ⊃ L2
  private type Constraint = (IRLifetime, IRLifetime);

  var constraints: Set[Constraint] = Set()

  def solve(program: IRProgram): Unit = {
    val bindings: Map[String, IRType] = program.methods.map(pair => pair._1 -> IRFuncPtr(pair._2.args.map(arg => arg._2), pair._2.retTyp))
                 ++ program.vmts.map(className => (className + "$_vmt") -> IRVmt(className)).toMap;
    program.methods.values.foreach((method) => {
      solve(method.body, bindings ++ method.args + (method.cont -> IRCont(List(method.retTyp))))
    });
  }

  def solve(exp: IRExp, bindings: Map[String, IRType]): Unit = {
    exp match {
      case IRLet(varName, typ, rhs, next) =>
        rhs match {
          case RhsDefC(args, body) => solve(body, bindings ++ args)
          case _ => ()
        }
        solve(next, bindings + (varName -> typ))
      case IRVar(varName, typ, rhs, next) =>
        solve(next, bindings + (varName -> typ))

      case IRAccess(varName, typ, root, label, next) =>
        solve(next, bindings + (varName -> typ));
        (typ, bindings(root)) match {
          case (IRAccessPtr(_, lt1), IRClass(_, lt2)) => constraints = constraints ++ Set((lt1, lt2), (lt2, lt1))
          case (IRAccessPtr(_, lt1), IRWrappedFunc(_, lt2)) => constraints = constraints ++ Set((lt1, lt2), (lt2, lt1))
        }
      case IRSet(varName, valueName, next) =>
        solve(next, bindings);
        (bindings(varName), bindings(valueName)) match {
          case (IRAccessPtr(expTyp, lt1), recTyp) =>
            addConstraint(recTyp, expTyp)
          case (IRVarPtr(expTyp, lt1), recTyp) =>
            addConstraint(recTyp, expTyp)
        }

      case IRCallC(cont, args) => bindings(cont) match {
        case IRCont(List(expArg)) => {
          addConstraint(bindings(args(0)), expArg)
        }
      }
      case IRCallF(func, cont, args) => (bindings(func), bindings(cont)) match {
        case (IRFuncPtr(argsTypes, ret), IRCont(List(contArg))) => {
          // TODO: Determine covariant vs contravariant
          argsTypes.zip(args).foreach(pair => addConstraint(bindings(pair._2), pair._1))
          addConstraint(ret, contArg)
        }
      }

      case IRIf(cond, contTrue, contFalse) => ()
      case IREOF() => ()
    }
  }

  def addConstraint(recTyp: IRType, expTyp: IRType): Unit = {
    // T@L1 <: S@L2 if and only if L1 ⊃ L2 and T <: S
    (recTyp, expTyp) match {
      case (IRClass(_, lt1), IRClass(_, lt2)) => constraints += (lt1, lt2)
      case (IRVarPtr(typ1, lt1), IRVarPtr(typ2, lt2)) => constraints += (lt1, lt2); addConstraint(typ1, typ2)
      case (IRWrappedFunc(_, lt1), IRWrappedFunc(_, lt2)) => constraints += (lt1, lt2)
      case _ => ()
    }
  }

}
