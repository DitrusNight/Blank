package blank

object ASTHelper {

  def freeVariables(exp: Expression): Set[String] = {
    exp match {
      case UnitLit(id) => Set()
      case IntLit(id, lit) => Set()
      case FloatLit(id, lit) => Set()
      case VarName(id, name) => Set(name)
      case PrimOp(id, op, args) => args.map(freeVariables).foldLeft(Set[String]())((acc, curr) => acc ++ curr)
      case AccessExp(id, root, label) => freeVariables(root)
      case FunctionCall(id, function, args) => freeVariables(function) ++ args.map(freeVariables).foldLeft(Set[String]())((acc, curr) => acc ++ curr)
      case IfStatement(id, cond, thenBr, elseBr) => freeVariables(cond) ++ freeVariables(thenBr) ++ freeVariables(elseBr)
      case LetBinding(id, varName, typ, rhs, next) => freeVariables(rhs) ++ (freeVariables(next) - varName)
      case VarBinding(id, varName, typ, rhs, next) => freeVariables(rhs) ++ (freeVariables(next) - varName)
      case ClassExpression(id, name, args, methods) => methods.values.foldLeft(Set[String]())((acc, curr) => acc ++ freeVariables(curr)) -- args.map((pair) => pair._1) - "this" -- methods.keySet;
      case LambdaExpression(id, attrs, args, retType, body) => freeVariables(body) -- args.map((pair) => pair._1);
    }
  }

}
