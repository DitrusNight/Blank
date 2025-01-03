package blank

object IROpt1 {

  def removeID(exp: IRExp): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        rhs match {
          case RhsPrim("id", List(arg)) => removeID(IRHelper.rename(varName, arg, next))
          case RhsDefF(cont, args, body, retTyp) => {
            IRLet(varName, typ, RhsDefF(cont, args, removeID(body), retTyp), removeID(next))
          }
          case RhsDefC(args, body) => {
            IRLet(varName, typ, RhsDefC(args, removeID(body)), removeID(next))
          }
          case _ => IRLet(varName, typ, rhs, removeID(next))
        }
      }
      case _ => exp
    }
  }

  def removeUnusedBindings(exp: IRExp, topLevel: Boolean): IRExp = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        val freeVars = IRHelper.freeVariables(next, Set());
        if(!freeVars.contains(varName) && !IRHelper.hasSideEffects(rhs) && !topLevel) {
          next
        } else {
          IRLet(varName, typ, removeUnusedBindings(rhs), removeUnusedBindings(next, topLevel))
        }
      }
      case _ => exp
    }
  }

  def removeUnusedBindings(rhs: IRRHS): IRRHS = {
    rhs match {
      case RhsDefF(cont, args, body, retTyp) => {
        RhsDefF(cont, args, removeUnusedBindings(body, false), retTyp)
      }
      case RhsDefC(args, body) => {
        RhsDefC(args, removeUnusedBindings(body, false))
      }
      case _ => rhs
    }
  }

  def performOptimizations(exp: IRExp): IRExp = {
    val noID = removeID(exp);
    val noUnused = removeUnusedBindings(noID, true);
    noUnused
  }

}
