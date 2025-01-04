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
case class RhsDeref(name: String) extends IRRHS {
  override def toString: String = "*" + name;
}
case class RhsDefF(cont: String, args: List[(String, IRType)], body: IRExp, retTyp: IRType) extends IRRHS {
  override def toString: String = "[" + cont +"](" + args.map((pair) => pair._1 + ": " + pair._2).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsDefC(args: List[(String, IRType)], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.map((pair) => (pair._1 + ": " + pair._2)).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsAlloc(typ: IRType) extends IRRHS {
  override def toString: String = "alloc(" + typ + ")";
}

abstract class IRType {
  def outputLLVM: String;
}
case class IRU8() extends IRType {
  override def toString: String = "u8";
  override def outputLLVM: String = "i8";
}
case class IRU16() extends IRType{
  override def toString: String = "u16";
  override def outputLLVM: String = "i16";
}
case class IRU32() extends IRType{
  override def toString: String = "u32";
  override def outputLLVM: String = "i32";
}
case class IRU64() extends IRType{
  override def toString: String = "u64";
  override def outputLLVM: String = "i64";
}
case class IRI8() extends IRType{
  override def toString: String = "i8";
  override def outputLLVM: String = "i8";
}
case class IRI16() extends IRType{
  override def toString: String = "i16";
  override def outputLLVM: String = "i16";
}
case class IRI32() extends IRType{
  override def toString: String = "i32";
  override def outputLLVM: String = "i32";
}
case class IRI64() extends IRType{
  override def toString: String = "i64";
  override def outputLLVM: String = "i64";
}
case class IRF32() extends IRType{
  override def toString: String = "f32";
  override def outputLLVM: String = "f32";
}
case class IRF64() extends IRType{
  override def toString: String = "f64";
  override def outputLLVM: String = "f64";
}
case class IRValPtr(typ: IRType) extends IRType {
  override def toString: String = "*" + typ;
  override def outputLLVM: String =
    (typ match {
      case struct@IRStruct(className, map) => LLVM.structTypes(struct)
      case _ => typ.outputLLVM
    }) + "*";
}
case class IRFuncPtr(args: List[IRType], ret: IRType) extends IRType {
  override def toString: String = "f(" + args.mkString(", ") + ") => " + ret;
  override def outputLLVM: String = "ptr";
}
case class IRBoolean() extends IRType {
  override def toString: String = "bool";
  override def outputLLVM: String = "i1";
}
case class IRUnit() extends IRType {
  override def toString: String = "unit";
  override def outputLLVM: String = "i1";
}
case class IRStruct(className: String, map: Map[String, IRType]) extends IRType {
  override def toString: String = "{ " + map.keySet.toList.map((elem) => elem + ": " + map(elem).toString).mkString(",") + " }";
  override def outputLLVM: String = "{ " + map.keySet.toList.map((elem) => map(elem).outputLLVM).mkString(", ") + " }";
}
case class IRUnk() extends IRType {
  override def toString: String = "unk";
  override def outputLLVM: String = "unk";
}
case class IRCont(args: List[IRType]) extends IRType {
  override def toString: String = "c(" + args.mkString(", ") + ")";
  override def outputLLVM: String = "label";
}

abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  def generateName(prefix: String = "name"): String = prefix + uniqInd();

  private def convertList(list: List[Expression], closureContext: Map[String, Expression], cont: (List[String]) => IRExp): IRExp = {
    var newCont = cont;
    for (arg <- list.reverse) {
      val prevCont = newCont;
      newCont = (elemNames: List[String]) => {
        convertASTToIR(generateName(), arg, closureContext, (elemName) => {
          prevCont(elemName :: elemNames)
        })
      }
    }
    newCont(List())
  }

  private def convertType(typ: Type): IRType = {
    typ match {
      case UnitType() => IRUnit()
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
      case FunType(args, ret) => IRFuncPtr(args.map(convertType), convertType(ret))
      case ClassType(className, fields, vmt, methods) => IRValPtr(IRStruct(className, fields.map(pair => pair._1 -> convertType(pair._2))))
      case _ => throw new RuntimeException("Unknown type to translate: " + typ);
    }
  }

  private def convertIDToIRType(id: ExpID): IRType = {
    convertType(ExpressionDataMap.getType(id))
  }

  private def wrapFunctionType(typ: IRFuncPtr, closure: IRType) = IRFuncPtr(closure :: typ.args, typ.ret)

  def convertASTToIR(name: String, exp: Expression, closureContext: Map[String, Expression], cont: (String) => IRExp): IRExp = {
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
      case VarName(id, oldName) => {
        closureContext.get(oldName) match {
          case Some(newExp) => convertASTToIR(name, newExp, closureContext, cont)
          case None => IRLet(name, convertIDToIRType(id), RhsPrim("id", List(oldName)), cont(name))
        }
      }
      case PrimOp(id, op, args) => {
        convertList(args, closureContext, (argNames) => {
          IRLet(name, convertIDToIRType(id), RhsPrim(op, argNames), cont(name))
        })
      }
      case LetBinding(id, varName, typ, rhs, next) => {
        convertASTToIR(varName, rhs, closureContext, (resName: String) => {
          convertASTToIR(generateName(), next, closureContext - varName, cont);
        })
      }
      case VarBinding(id, varName, typ, rhs, next) => {
        val newTyp = convertType(typ);
        IRLet(varName, IRValPtr(newTyp), RhsAlloc(newTyp),
          convertASTToIR(
            generateName(),
            rhs,
            closureContext,
            rhsName => {
              IRLet(generateName(), newTyp, RhsPrim("=", List(varName, rhsName)), convertASTToIR(generateName(), next, closureContext - varName, cont))
            }
          )
        )
      }
      case IfStatement(id, cond, thenBr, elseBr) => {
        convertASTToIR(generateName("cond"), cond, closureContext, (condName: String) => {
          val res = generateName("res");
          val finallyCont = generateName("finCont");
          val thenCont = generateName("thenCont");
          val elseCont = generateName("elseCont");
          val argTyp = convertIDToIRType(id);
          IRLet(finallyCont, IRCont(List(argTyp)), RhsDefC(List((res, argTyp)), IRLet(name, convertIDToIRType(id), RhsPrim("id", List(res)), cont(res))),
            IRLet(thenCont, IRCont(List()), RhsDefC(List(), convertASTToIR(generateName(), thenBr, closureContext, (res) => IRCallC(finallyCont, List(res)))),
              IRLet(elseCont, IRCont(List()), RhsDefC(List(), convertASTToIR(generateName(), elseBr, closureContext, (res) => IRCallC(finallyCont, List(res)))),
                IRIf(condName, thenCont, elseCont)
              )
            )
          )
        })
      }
      case AccessExp(id, root, label) => {
        val typ = convertIDToIRType(id);
        val ptrName = generateName();
        convertASTToIR(generateName(), root, closureContext, (rootName: String) => {
          IRLet(ptrName, IRValPtr(typ), RhsAccess(rootName, label),
            IRLet(name, typ, RhsDeref(ptrName), cont(name)))
        });
      }
      case FunctionCall(id, function, args) => {
        function match {
          case AccessExp(accessId, root, label) => {
            val typ = convertIDToIRType(root.getID);
            typ match {
              case IRValPtr(IRStruct(className, map)) => {
                convertASTToIR(name, FunctionCall(id, VarName(accessId, className + "$" + label), root :: args), closureContext, cont)
              }
            }
          }
          case _ => {
            convertASTToIR(generateName(), function, closureContext, (funName) => {
              convertList(args, closureContext, (argsNames) => {
                val contName = generateName("cont");
                val ret = generateName("ret");
                val argTyp = convertIDToIRType(id);
                IRLet(contName, IRCont(List(argTyp)), RhsDefC(List((ret, argTyp)), {
                  IRLet(name, convertIDToIRType(id), RhsPrim("id", List(ret)), cont(name))
                }),
                  IRCallF(funName, contName, argsNames)
                )
              })
            })
          }
        }
      }
      case LambdaExpression(id, args, retType, body) => {
        val contName = generateName("cont");
        IRLet(name, IRFuncPtr(args.map(elem => convertType(elem._2)), convertType(retType)), RhsDefF(contName, args.map(elem => (elem._1, convertType(elem._2))),
          convertASTToIR(generateName(), body, closureContext -- args.map((pair) => pair._1), (resName: String) =>
            IRCallC(contName, List(resName))
          ), convertType(ExpressionDataMap.getType(id) match {
            case FunType(args, ret) => ret
          })), cont(name)
        );
      }
      case ClassExpression(id, args, map) => {

        def reduce(list: List[(String, Type)], ptrName: String, cont: () => IRExp): IRExp = {
          var newCont = cont;
          for (pair <- list.reverse) {
            val prevCont = newCont;
            val currPair = pair;
            newCont = () => {
              val fieldPtr = generateName();
              val typ = convertType(currPair._2)
              IRLet(fieldPtr, IRValPtr(typ), RhsAccess(ptrName, currPair._1),
                IRLet(generateName(), typ, RhsPrim("=", List(fieldPtr, currPair._1)), prevCont())
              )
            }
          }
          newCont()
        }
        /*
        def reduceMethods(list: List[(String, LambdaExpression)], ptrName: String, cont: () => IRExp): IRExp = {
          var newCont = cont;
          for (pair <- list.reverse) {
            val prevCont = newCont;
            val currPair = pair;
            newCont = () => {
              val fieldPtr = generateName();
              val typ = IRFuncPtr(currPair._2.args.map(pair => convertType(pair._2)), convertType(currPair._2.retType))
              IRLet(fieldPtr, IRValPtr(typ), RhsAccess(ptrName, currPair._1),
                IRLet(generateName(), typ, RhsPrim("=", List(fieldPtr, name + "$" + currPair._1)), prevCont())
              )
            }
          }
          newCont()
        }*/
        val funcTyp = convertIDToIRType(id)
        val classTyp = funcTyp match { case IRFuncPtr(args, ret) => ret };
        val classCont = generateName("cont");

        val resName = generateName();
        val fieldMap = classTyp match {
          case IRValPtr(IRStruct(className, map)) => map
        };

        val closureName = generateName("closure");
        var newCont = () => {
          IRLet(name, funcTyp, RhsDefF(classCont, args.map(elem => (elem._1, convertType(elem._2))),
            IRLet(resName, classTyp, RhsAlloc(classTyp match { case IRValPtr(typ) => typ }),
              reduce(args, resName, () => {
                //reduceMethods(map.toList, resName, () => {
                  IRCallC(classCont, List(resName))
                //})
              })
            ), classTyp), cont(name))
        };

        val varId = ExpressionDataMap.cloneID(id);

        ExpressionDataMap.putType(varId, ExpressionDataMap.getType(varId) match {
          case FunType(args, classTyp@ClassType(className, fields, vmt, methods)) => classTyp
        });

        val newClosureCtx = closureContext ++ args.map(
          (argPair) => {
            val accessId = ExpressionDataMap.cloneID(id);

            ExpressionDataMap.putType(accessId, ExpressionDataMap.getType(accessId) match {
              case FunType(args, classTyp@ClassType(className, fields, vmt, methods)) => {
                if(fields.contains(argPair._1)) {
                  fields(argPair._1)
                } else if(vmt.contains(argPair._1)) {
                  vmt(argPair._1)
                } else {
                  methods(argPair._1)
                }
              }
            });

            (argPair._1 -> (AccessExp(accessId, VarName(varId, closureName), argPair._1)))
          }
        );

        for(pair <- map) {
          val contName = generateName("cont");
          val lambda = pair._2;

          val defF = RhsDefF(contName, (closureName, classTyp) :: lambda.args.map(elem => (elem._1, convertType(elem._2))),
            convertASTToIR(generateName(), lambda.body, newClosureCtx, (resName: String) =>
              IRCallC(contName, List(resName))
            ), convertType(lambda.retType))
          val currCont = newCont;
          val currPair = pair;
          newCont = () => IRLet(name + "$" + currPair._1, IRFuncPtr(classTyp :: lambda.args.map(elem => convertType(elem._2)), convertType(lambda.retType)), defF, currCont())
        }

        newCont();
      }
    }
  }

}
