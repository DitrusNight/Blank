package blank

abstract class IRExp
case class IRLet(varName: String, typ: IRType, rhs: IRRHS, next: IRExp) extends IRExp {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class IRCallF(func: String, cont: String, args: List[String]) extends IRExp {
  override def toString: String = func + "[" + cont + "](" + args.mkString(", ") + ")";
}
case class IRCallC(cont: String, arg: String) extends IRExp {
  override def toString: String = cont + "(" + arg + ")";
}
case class IREOF() extends IRExp {
  override def toString: String = "";
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
case class RhsDefF(cont: String, args: List[(String, IRType)], body: IRExp, retTyp: IRType) extends IRRHS {
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
  override def toString: String = "i8";
}
case class IRU16() extends IRType{
  override def toString: String = "i16";
}
case class IRU32() extends IRType{
  override def toString: String = "i32";
}
case class IRU64() extends IRType{
  override def toString: String = "i64";
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
case class IRPtr() extends IRType {
  override def toString: String = "ptr";
}
case class IRBoolean() extends IRType{
  override def toString: String = "i1";
}
case class IRUnit() extends IRType{
  override def toString: String = "{}";
}
/*case class IRPtr(typ: IRType, props: PtrProps) extends IRType {
  override def toString: String = "*[" + props + "]" + typ;
}
case class IRStruct(map: Map[String, IRType]) extends IRType {
  override def toString: String = "{\n" + map.keySet.toList.map((elem) => "  " + elem + ": " + map(elem)).mkString(",\n") + "}";
}*/
// TODO: Make typing system.
case class IRUnk() extends IRType {
  override def toString: String = "unk";
}
case class IRCont(argType: IRType) extends IRType {
  override def toString: String = "(" + argType + ") => ()";
}

abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  def generateName(prefix: String = "name") = prefix + uniqInd();

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

  private def convertType(typ: Type): IRType = {
    typ match {
      case BaseType("boolean") => IRBoolean()
      case BaseType("u8") => IRU8()
      case BaseType("u16") => IRU16()
      case BaseType("u32") => IRU32()
      case BaseType("u64") => IRU64()
      case BaseType("i8") => IRI8()
      case BaseType("i16") => IRI16()
      case BaseType("i32") => IRI32()
      case BaseType("i64") => IRI64()
      case BaseType("f32") => IRF32()
      case BaseType("f64") => IRF64()
      case FunType(args, ret) => IRPtr()
      case _ => IRUnk()
    }
  }

  def convertIDToIRType(id: ExpID): IRType = {
    convertType(ExpressionDataMap.getType(id))
  }

  def convertASTToIR(name: String, exp: Expression, cont: (String) => IRExp): IRExp = {
    exp match {
      case IntLit(id, lit) => {
        IRLet(name, convertIDToIRType(id), RhsIntLit(lit), cont(name))
      }
      case FloatLit(id, lit) => {
        IRLet(name, convertIDToIRType(id), RhsFloatLit(lit), cont(name))
      }
      case UnitLit(id) => {
        IRLet(name, IRUnit(), RhsUnitLit(), cont(name))
      }
      case VarName(id, name) => {
        cont(name)
      }
      case PrimOp(id, op, args) => {
        convertList(args, (argNames) => {
          IRLet(name, convertIDToIRType(id), RhsPrim(op, argNames), cont(name))
        })
      }
      case LetBinding(id, varName, typ, rhs, next) => {
        convertASTToIR(varName, rhs, (resName: String) => {
          convertASTToIR(generateName(), next, cont);
        })
      }
      case IfStatement(id, cond, thenBr, elseBr) => {
        convertASTToIR(generateName("cond"), cond, (condName: String) => {
          val res = generateName("res");
          val finallyCont = generateName("finCont");
          val thenCont = generateName("thenCont");
          val elseCont = generateName("elseCont");
          IRLet(finallyCont, IRCont(convertIDToIRType(id)), RhsDefC(List(res), IRLet(name, convertIDToIRType(id), RhsPrim("id", List(res)), cont(res))),
            IRLet(thenCont, IRCont(IRUnk()), RhsDefC(List(), convertASTToIR(generateName(), thenBr, (res) => IRCallC(finallyCont, res))),
              IRLet(elseCont, IRCont(IRUnk()), RhsDefC(List(), convertASTToIR(generateName(), elseBr, (res) => IRCallC(finallyCont, res))),
                IRIf(condName, thenCont, elseCont)
              )
            )
          )
        })
      }
      case AccessExp(id, root, label) => {
        convertASTToIR(generateName(), root, (rootName: String) => {
          IRLet(name, convertIDToIRType(id), RhsAccess(rootName, label), cont(name))
        });
      }
      case FunctionCall(id, function, args) => {
        convertASTToIR(generateName(), function, (funName) => {
          convertList(args, (argsNames) => {
            val contName = generateName("cont");
            val ret = generateName("ret");
            IRLet(contName, IRCont(convertIDToIRType(id)), RhsDefC(List(ret), {
              IRLet(name, convertIDToIRType(id), RhsPrim("id", List(ret)), cont(name))
            }),
              IRCallF(funName, contName, argsNames)
            )
          })
        })
      }
      case LambdaExpression(id, args, retType, body) => {
        val contName = generateName("cont");
        IRLet(name, IRPtr(), RhsDefF(contName, args.map(elem => (elem._1, convertType(elem._2))),
          convertASTToIR(generateName(), body, (resName: String) =>
            IRCallC(contName, resName)
          ), convertType(ExpressionDataMap.getType(id) match {
            case FunType(args, ret) => ret
          })), cont(name)
        );
      }
      case ClassExpression(id, args, body) => {
        /*
        val lambda = generateName("lambda");
        val contName = generateName("cont");
        IRLet(lambda, IRUnk(), RhsDefF(contName, args.map(elem => elem._1), convertASTToIR(body, (resName: String) =>
          IRCallC(contName, List(resName))
        )), cont(lambda));
        */
        val resName = generateName("cont");
        IRLet(resName, convertIDToIRType(id), RhsUnitLit(), cont(resName));
      }
    }
  }

}
