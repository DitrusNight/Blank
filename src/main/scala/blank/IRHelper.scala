package blank

object IRHelper {

  def freeVariables(exp: IRExp, bound: Set[String]): Set[String] = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        freeVariables(rhs, bound) ++ freeVariables(next, bound + varName);
      }
      case IRCallF(func, cont, args) => {
        (args.toSet + cont) -- bound
      }
      case IRCallC(func, args) => {
        (args.toSet) -- bound
      }
      case IREOF() => Set()
    }
  }

  def freeVariables(rhs: IRRHS, bound: Set[String]): Set[String] = {
    rhs match {
      case RhsDefF(cont, args, body, retTyp) => {
        freeVariables(body, bound ++ args.map((elem) => elem._1))
      }
      case _ => Set()
    }
  }

  def hasSideEffects(rhs: IRRHS): Boolean = {
    rhs match {
      case RhsPrim("=", args) => true
      case _ => false
    }
  }

  def rename(name: String, newName: String, exp: IRExp): IRExp = {
    def replace(str: String): String = if(str == name) newName else str;
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        if(varName != name) {
          IRLet(varName, typ, rename(name, newName, rhs), rename(name, newName, next))
        } else {
          IRLet(varName, typ, rename(name, newName, rhs), next)
        }
      }
      case IRCallC(cont, args) => {
        IRCallC(replace(cont), args.map((str) => replace(str)))
      }
      case IRCallF(func, cont, args) => {
        IRCallF(replace(func), replace(cont), args.map((str) => replace(str)))
      }
      case IRIf(cond, contTrue, contFalse) => {
        IRIf(replace(cond), replace(contTrue), replace(contFalse))
      }
    }
  }

  def rename(name: String, newName: String, rhs: IRRHS): IRRHS = {
    def replace(str: String): String = if(str == name) newName else str;
    def replaceList(list: List[String]): List[String] = list.map((str) => replace(str));
    rhs match {
      case RhsDefF(cont, args, body, retTyp) => {
        if(cont == name || args.map((pair) => pair._1).contains(name)) {
          rhs
        } else {
          RhsDefF(cont, args, rename(name, newName, body), retTyp)
        }
      }
      case RhsDefC(args, body) => {
        if(args.map((pair) => pair._1).contains(name)) {
          rhs
        } else {
          RhsDefC(args, rename(name, newName, body))
        }
      }
      case RhsPrim(op, args) => {
        RhsPrim(op, replaceList(args))
      }
      case RhsAccess(root, label) => RhsAccess(replace(root), label)

      // No-ops
      case RhsIntLit(lit) => rhs
      case RhsFloatLit(lit) => rhs
      case RhsUnitLit() => rhs
      case RhsStringLit(lit) => rhs
      case RhsAlloc(typ) => rhs
    }
  }

}
