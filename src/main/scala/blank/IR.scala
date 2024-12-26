package blank

abstract class IRExp
case class IRLet(varName: String, typ: IRType, rhs: IRRHS, next: IRExp) extends IRExp {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
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
  override def toString: String = "[" + cont +"](" + args.mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsDefC(args: List[String], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsAlloc(typ: IRType) extends IRRHS {
  override def toString: String = "alloc(" + typ + ")";
}

abstract class IRType
case class IRU8() extends IRType {
  override def toString: String = "u8";
}
case class IRU16() extends IRType{
  override def toString: String = "u16";
}
case class IRU32() extends IRType{
  override def toString: String = "u32";
}
case class IRU64() extends IRType{
  override def toString: String = "u64";
}
case class IRI8() extends IRType{
  override def toString: String = "i8";
}
case class IRI16() extends IRType{
  override def toString: String = "i16";
}
case class IRI32() extends IRType{
  override def toString: String = "i32";
}
case class IRI64() extends IRType{
  override def toString: String = "i64";
}
case class IRF32() extends IRType{
  override def toString: String = "f32";
}
case class IRF64() extends IRType{
  override def toString: String = "f64";
}
case class IRBoolean() extends IRType{
  override def toString: String = "boolean";
}
case class IRUnit() extends IRType{
  override def toString: String = "{}";
}
case class IRPtr(typ: IRType, props: PtrProps) extends IRType {
  override def toString: String = "*[" + props + "]" + typ;
}
case class IRStruct(map: Map[String, IRType]) extends IRType {
  override def toString: String = "{\n" + map.keySet.toList.map((elem) => "  " + elem + ": " + map(elem)).mkString(",\n") + "}";
}
// TODO: Make typing system.
case class IRUnk() extends IRType {
  override def toString: String = "unknown";
}

abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  private def generateName(prefix: String = "name") = prefix + uniqInd();

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
      case IntLit(lit) => {
        val name = generateName();
        IRLet(name, IRI64(), RhsIntLit(lit), cont(name))
      }
      case FloatLit(lit) => {
        val name = generateName();
        IRLet(name, IRF64(), RhsFloatLit(lit), cont(name))
      }
      case UnitLit() => {
        val name = generateName();
        IRLet(name, IRUnit(), RhsUnitLit(), cont(name))
      }
      case VarName(name) => {
        cont(name)
      }
      case PrimOp(op, args) => {
        convertList(args, (argNames) => {
          val resName = generateName();
          IRLet(resName, IRUnk(), RhsPrim(op, argNames), cont(resName))
        })
      }
      case LetBinding(varName, typ, rhs, next) => {
        convertASTToIR(rhs, (resName: String) => {
          IRLet(varName, IRUnk(), RhsPrim("id", List(resName)), convertASTToIR(next, identityCont));
        })
      }
      case AccessExp(root, label) => {
        val name = generateName();
        convertASTToIR(root, (rootName: String) => {
          IRLet(name, IRUnk(), RhsAccess(rootName, label), cont(name))
        });
      }
      case FunctionCall(function, args) => {
        convertASTToIR(function, (funName) => {
          convertList(args, (argsNames) => {
            val contName = generateName("cont");
            val ret = generateName("ret");
            IRLet(contName, IRUnk(), RhsDefC(List(ret), cont(ret)),
              IRCallF(funName, contName, argsNames)
            )
          })
        })
      }
      case LambdaExpression(args, retType, body) => {
        val lambda = generateName("lambda");
        val contName = generateName("cont");
        IRLet(lambda, IRUnk(), RhsDefF(contName, args.map(elem => elem._1), convertASTToIR(body, (resName: String) =>
          IRCallC(contName, List(resName))
        )), cont(lambda));
      }
    }
  }

}
