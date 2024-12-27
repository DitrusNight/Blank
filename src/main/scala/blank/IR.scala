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
  override def toString: String = "i32";
}

abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  def generateName(prefix: String = "name") = prefix + uniqInd();

  def identityCont = (res: String) => IRValue(res)

  private def convertList(list: List[Expression], cont: (List[String]) => IRExp): IRExp = {
    var newCont = cont;
    for (arg <- list.reverse) {
      val prevCont = newCont;
      newCont = (elemNames: List[String]) => {
        convertASTToIR(generateName(), arg, (elemName) => {
          prevCont(elemName :: elemNames)
        })
      }
    }
    newCont(List())
  }

  def convertASTToIR(name: String, exp: Expression, cont: (String) => IRExp): IRExp = {
    exp match {
      case IntLit(lit) => {
        IRLet(name, IRI32(), RhsIntLit(lit), cont(name))
      }
      case FloatLit(lit) => {
        IRLet(name, IRF64(), RhsFloatLit(lit), cont(name))
      }
      case UnitLit() => {
        IRLet(name, IRUnit(), RhsUnitLit(), cont(name))
      }
      case VarName(name) => {
        cont(name)
      }
      case PrimOp(op, args) => {
        convertList(args, (argNames) => {
          IRLet(name, IRUnk(), RhsPrim(op, argNames), cont(name))
        })
      }
      case LetBinding(varName, typ, rhs, next) => {
        convertASTToIR(varName, rhs, (resName: String) => {
          convertASTToIR(generateName(), next, identityCont);
        })
      }
      case AccessExp(root, label) => {
        convertASTToIR(generateName(), root, (rootName: String) => {
          IRLet(name, IRUnk(), RhsAccess(rootName, label), cont(name))
        });
      }
      case FunctionCall(function, args) => {
        convertASTToIR(generateName(), function, (funName) => {
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
        val contName = generateName("cont");
        IRLet(name, IRUnk(), RhsDefF(contName, args.map(elem => elem._1),
          convertASTToIR(generateName(), body, (resName: String) =>
          IRCallC(contName, List(resName))
        )), cont(name));
      }
      case ClassExpression(args, body) => {
        /*
        val lambda = generateName("lambda");
        val contName = generateName("cont");
        IRLet(lambda, IRUnk(), RhsDefF(contName, args.map(elem => elem._1), convertASTToIR(body, (resName: String) =>
          IRCallC(contName, List(resName))
        )), cont(lambda));
        */
        val resName = generateName("cont");
        IRLet(resName, IRUnit(), RhsUnitLit(), cont(resName));
      }
    }
  }

}
