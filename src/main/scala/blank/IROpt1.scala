package blank

object IROpt1 {

  private def removeID(exp: IRExp): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        rhs match {
          case RhsPrim("id", List(arg)) => removeID(IRHelper.rename(varName, arg, next))
          /*case RhsDefF(cont, attrs, args, body, retTyp) => {
            IRLet(varName, typ, RhsDefF(cont, attrs, args, removeID(body, false), retTyp), removeID(next, topLevel))
          }*/
          case RhsDefC(args, body) => {
            IRLet(varName, typ, RhsDefC(args, removeID(body)), removeID(next))
          }
          case _ => IRLet(varName, typ, rhs, removeID(next))
        }
      }
      case IRVar(varName, typ, rhs, next) => {
        IRVar(varName, typ, rhs, removeID(next))
      }
      case IRAccess(varName, typ, root, label, next) => {
        val freeVars = IRHelper.freeVariables(next);
        if(!freeVars.contains(varName)) {
          removeID(next)
        } else {
          IRAccess(varName, typ, root, label, removeID(next))
        }
      }
      case IRSet(varName, valueName, next) => {
        IRSet(varName, valueName, removeID(next))
      }
      case IRCallF(func, cont, args) => exp
      case IRCallC(cont, args) => exp
      case IRIf(cond, contTrue, contFalse) => exp
      case IREOF() => exp
    }
  }

  private def removeUnusedBindings(exp: IRExp): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        val freeVars = IRHelper.freeVariables(next);
        if(!freeVars.contains(varName)) {
          removeUnusedBindings(next)
        } else {
          IRLet(varName, typ, removeUnusedBindings(rhs), removeUnusedBindings(next))
        }
      }
      case IRVar(varName, typ, rhs, next) => {
        val freeVars = IRHelper.freeVariables(next);
        if(!freeVars.contains(varName)) {
          removeUnusedBindings(next)
        } else {
          IRVar(varName, typ, removeUnusedBindings(rhs), removeUnusedBindings(next))
        }
      }
      case IRAccess(varName, typ, root, label, next) => {
        val freeVars = IRHelper.freeVariables(next);
        if(!freeVars.contains(varName)) {
          removeUnusedBindings(next)
        } else {
          IRAccess(varName, typ, root, label, removeUnusedBindings(next))
        }
      }
      case IRSet(varName, valueName, next) => {
        IRSet(varName, valueName, removeUnusedBindings(next))
      }
      case IRCallC(cont, args) => exp
      case IRCallF(func, cont, args) => exp
      case IREOF() => exp
    }
  }

  private def removeUnusedBindings(rhs: IRRHS): IRRHS = {
    rhs match {
      /*case RhsDefF(cont, attrs, args, body, retTyp) => {
        RhsDefF(cont, attrs, args, removeUnusedBindings(body, false), retTyp)
      }*/
      case RhsDefC(args, body) => {
        RhsDefC(args, removeUnusedBindings(body))
      }
      case _ => rhs
    }
  }

  private def tailRec(exp: IRExp): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        rhs match {
          /*case RhsDefF(cont, attrs, args, body, retTyp) => {
            IRLet(varName, typ, RhsDefF(cont, attrs, args, tailRec(body), retTyp), tailRec(next))
          }*/
          case RhsDefC(defArgs, body) => {
            body match {
              case IRCallC(cont, args) => {
                if(args == defArgs.map((elem) => elem._1)) {
                  tailRec(IRHelper.rename(varName, cont, next))
                } else {
                  IRLet(varName, typ, rhs, tailRec(next))
                }
              }
              case _ => IRLet(varName, typ, RhsDefC(defArgs, tailRec(body)), tailRec(next))
            }
          }
          case _ => IRLet(varName, typ, rhs, tailRec(next))
        }
      }
      case _ => exp
    }
  }

  def performOptimizations(program: IRProgram): Unit = {
    program.methods = program.methods.map((method) => {
      val exp = method._2.body
      val noID = removeID(exp);
      val noUnused = removeUnusedBindings(noID);
      val optTailRec = tailRec(noUnused);
      method._1 -> IRFunction(method._2.cont, method._2.args, optTailRec, method._2.retTyp)
    })
  }

}
