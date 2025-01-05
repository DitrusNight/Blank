package blank

object IROpt1 {

  private def removeID(exp: IRExp, topLevel: Boolean): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        rhs match {
          case RhsPrim("id", List(arg)) => if(topLevel) {
            IRLet(varName, typ, rhs, removeID(next, topLevel))
          } else {
            removeID(IRHelper.rename(varName, arg, next), topLevel)
          }
          case RhsDefF(cont, attrs, args, body, retTyp) => {
            IRLet(varName, typ, RhsDefF(cont, attrs, args, removeID(body, false), retTyp), removeID(next, topLevel))
          }
          case RhsDefC(args, body) => {
            IRLet(varName, typ, RhsDefC(args, removeID(body, topLevel)), removeID(next, topLevel))
          }
          case _ => IRLet(varName, typ, rhs, removeID(next, topLevel))
        }
      }
      case _ => exp
    }
  }

  private def removeUnusedBindings(exp: IRExp, topLevel: Boolean): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        val freeVars = IRHelper.freeVariables(next);
        if(!freeVars.contains(varName) && !IRHelper.hasSideEffects(rhs) && !topLevel) {
          next
        } else {
          IRLet(varName, typ, removeUnusedBindings(rhs), removeUnusedBindings(next, topLevel))
        }
      }
      case _ => exp
    }
  }

  private def removeUnusedBindings(rhs: IRRHS): IRRHS = {
    rhs match {
      case RhsDefF(cont, attrs, args, body, retTyp) => {
        RhsDefF(cont, attrs, args, removeUnusedBindings(body, false), retTyp)
      }
      case RhsDefC(args, body) => {
        RhsDefC(args, removeUnusedBindings(body, false))
      }
      case _ => rhs
    }
  }

  private def tailRec(exp: IRExp): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        rhs match {
          case RhsDefF(cont, attrs, args, body, retTyp) => {
            IRLet(varName, typ, RhsDefF(cont, attrs, args, tailRec(body), retTyp), tailRec(next))
          }
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

  def performOptimizations(exp: IRExp): IRExp = {
    val noID = removeID(exp, true);
    val noUnused = removeUnusedBindings(noID, true);
    val optTailRec = tailRec(noUnused);
    optTailRec
  }

}
