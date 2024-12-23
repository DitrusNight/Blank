package blank

abstract class IRExp
case class IRLet(varName: String, rhs: IRRHS, next: IRExp) extends IRExp {
  override def toString: String = "let " + varName + " = " + rhs + ";\n" + next;
}
case class IRCallF(func: String, cont: String, args: List[String]) extends IRExp {
  override def toString: String = func + "[" + cont + "](" + args.mkString(", ") + ")";
}
case class IRCallC(cont: String, args: List[String]) extends IRExp {
  override def toString: String = cont + "(" + args.mkString(", ") + ")";
}
case class IRValue(varName: String) extends IRExp {
  override def toString: String = varName;
}
case class IRIf(cond: String, contTrue: String, contFalse: String) extends IRExp {
  override def toString: String = "if(" + cond + ") {\n  " + contTrue + "();\n} else {\n  " + contFalse + "();\n}";
}

abstract class IRRHS
case class RhsIntLit(lit: Int) extends IRRHS {
  override def toString: String = lit.toString;
}
case class RhsFloatLit(lit: Float) extends IRRHS {
  override def toString: String = lit.toString;
}
case class RhsStringLit(lit: String) extends IRRHS {
  override def toString: String = '"' + lit + '"';
}
case class RhsUnitLit() extends IRRHS {
  override def toString: String = "{}";
}
// TODO Change prim op to enum.
case class RhsPrim(op: String, args: List[String]) extends IRRHS {
  override def toString: String = op + "(" + args.mkString(", ") + ")";
}
case class RhsAccess(root: String, label: String) extends IRRHS {
  override def toString: String = root + "." + label;
}
case class RhsDefF(cont: String, args: List[String], body: IRExp) extends IRRHS {
  override def toString: String = "[" + cont +"](" + args.mkString(", ") + ") {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsDefC(args: List[String], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.mkString(", ") + ") {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsAlloc() extends IRRHS {
  override def toString: String = "alloc ";
}

object IR {

  private var uniqueIndex = 0;
  private def uniqInd: Int = {
    uniqueIndex += 1;
    uniqueIndex
  }
  private def generateName(prefix: String = "name") = prefix + uniqInd;

  def identityCont = (res: String) => IRValue(res)

  private def convertList(list: List[Expression], cont: (List[String]) => IRExp): IRExp = {
    var newCont = cont;
    for (arg <- list.reverse) {
      val prevCont = newCont;
      newCont = (elemNames: List[String]) => {
        convertASTToIR(arg, (elemName) => {
          prevCont(elemName :: elemNames)
        })
      }
    }
    newCont(List())
  }

  def convertASTToIR(exp: Expression, cont: (String) => IRExp): IRExp = {
    exp match {
      case Lit(lit) => {
        val name = generateName();
        IRLet(name, RhsIntLit(Integer.parseInt(lit)), cont(name))
      }
      case UnitLit() => {
        val name = generateName();
        IRLet(name, RhsUnitLit(), cont(name))
      }
      case VarName(name) => {
        cont(name)
      }
      case PrimOp(op, args) => {
        convertList(args, (argNames) => {
          val resName = generateName();
          IRLet(resName, RhsPrim(op, argNames), cont(resName))
        })
      }
      case LetBinding(varName, typ, rhs, next) => {
        convertASTToIR(rhs, (resName: String) => {
          IRLet(varName, RhsPrim("id", List(resName)), convertASTToIR(next, identityCont));
        })
      }
      case AccessExp(root, label) => {
        val name = generateName();
        convertASTToIR(root, (rootName: String) => {
          IRLet(name, RhsAccess(rootName, label), cont(name))
        });
      }
      case FunctionCall(function, args) => {
        convertASTToIR(function, (funName) => {
          convertList(args, (argsNames) => {
            val contName = generateName("cont");
            val ret = generateName("ret");
            IRLet(contName, RhsDefC(List(ret), cont(ret)),
              IRCallF(funName, contName, argsNames)
            )
          })
        })
      }
      case LambdaExpression(args, retType, body) => {
        val lambda = generateName("lambda");
        val contName = generateName("cont");
        IRLet(lambda, RhsDefF(contName, args.map(elem => elem._1), convertASTToIR(body, (resName: String) =>
          IRCallC(contName, List(resName))
        )), cont(lambda));
      }
    }
  }

}
