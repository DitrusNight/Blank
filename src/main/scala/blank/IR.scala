package blank

abstract class IRExp
case class IRLet(varName: String, typ: IRType, rhs: IRRHS, next: IRExp) extends IRExp {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class IRVar(varName: String, typ: IRType, rhs: IRRHS, next: IRExp) extends IRExp {
  override def toString: String = "var " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class IRAccess(varName: String, typ: IRType, root: String, label: String, next: IRExp) extends IRExp {
  override def toString: String = "var " + varName + ": " + typ + " = &(" + root  + "->" + label + ");\n" + next;
}
case class IRSet(varName: String, valueName: String, next: IRExp) extends IRExp {
  override def toString: String = varName + " = " + valueName + ";\n" + next;
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
case class RhsDeref(name: String) extends IRRHS {
  override def toString: String = "*" + name;
}
case class RhsDefF(cont: String, attrs: List[String], args: List[(String, IRType)], body: IRExp, retTyp: IRType) extends IRRHS {
  override def toString: String = attrs.mkString("|") + "[" + cont +"](" + args.map((pair) => pair._1 + ": " + pair._2).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsDefC(args: List[(String, IRType)], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.map((pair) => (pair._1 + ": " + pair._2)).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsClassAlloc(typ: IRType) extends IRRHS {
  override def toString: String = "alloc(" + typ + ")";
}



abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  def generateName(prefix: String = "name"): String = prefix + uniqInd();

  private def convertList(list: List[Expression], bindings: Map[String, IRType], closureContext: Map[String, Expression], cont: (List[String], Map[String, IRType]) => IRExp): IRExp = {
    var newCont = cont;
    for (arg <- list.reverse) {
      val prevCont = newCont;
      newCont = (elemNames: List[String], bindings) => {
        convertASTToIR(arg, bindings, closureContext, (elemName, bindings) => {
          prevCont(elemName :: elemNames, bindings)
        })
      }
    }
    newCont(List(), bindings)
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
      case FunType(attrs, args, ret) => IRFuncPtr(attrs, args.map(convertType), convertType(ret))
      case ClassType(className, fields, methods) => {
        IRTypes.vmtMap = IRTypes.vmtMap + (className -> (generateName("vmt"), VmtStruct(
          methods.filter(pair => pair._2.attrs.contains("virtual")).map((pair) => pair._1 -> (
            convertType(pair._2) match {
              case funcPtr@IRFuncPtr(attrs, args, retTyp) => IRFuncPtr(attrs, IRClass(className) :: args, retTyp)
            }))
        )))
        IRTypes.classMap = IRTypes.classMap + (className -> (generateName("class"), ClassStruct(
          fields.map(pair => pair._1 -> convertType(pair._2)),
          methods
            .filter(pair => !pair._2.attrs.contains("virtual"))
            .map((pair) => pair._1 -> ((
              convertType(pair._2) match {
                case funcPtr@IRFuncPtr(attrs, args, retTyp) => IRFuncPtr(attrs, IRClass(className) :: args, retTyp)
              }))),
          IRVmt(className),
        )));
        IRClass(className)
      }
      case _ => throw new RuntimeException("Unknown type to translate: " + typ);
    }
  }

  private def convertIDToIRType(id: ExpID): IRType = {
    convertType(ExpressionDataMap.getType(id))
  }

  def convertASTToIR(exp: Expression, bindings: Map[String, IRType], closureContext: Map[String, Expression], cont: (String, Map[String, IRType]) => IRExp, requestedName: String = "", refReqested: Boolean = false): IRExp = {
    exp match {
      case IntLit(id, lit) => {
        val name = generateName("int");
        IRLet(name, convertIDToIRType(id), RhsIntLit(lit), cont(name, bindings))
      }
      case FloatLit(id, lit) => {
        val name = generateName("float");
        IRLet(name, convertIDToIRType(id), RhsFloatLit(lit), cont(name, bindings))
      }
      case UnitLit(id) => {
        val name = generateName("unit");
        IRLet(name, IRUnit(), RhsUnitLit(), cont(name, bindings))
      }
      case VarName(id, oldName) => {
        closureContext.get(oldName) match {
          case Some(newExp) => convertASTToIR(newExp, bindings, closureContext, cont)
          case None => bindings.get(oldName) match {
            case Some(IRVarPtr(typ)) => {
              if (refReqested) {
                cont(oldName, bindings)
              } else {
                val name = generateName("deref")
                IRLet(name, typ, RhsDeref(oldName), cont(name, bindings))
              }
            }
            case Some(typ) => cont(oldName, bindings)
            case None => ErrorHandler.raiseError(id, "Unknown variable: " + oldName); cont("", bindings)
          }
        }
      }
      case PrimOp(id, op, args) => {
        println(args);
        val name = generateName("prim");
        if (op == "=") {
          convertASTToIR(args(0), bindings, closureContext, (refName, bindings) => {
            convertASTToIR(args(1), bindings, closureContext, (valueName, bindings) => {
              IRSet(refName, valueName,
                cont(valueName, bindings)
              )
            })
          }, "", true)
        } else {
          convertList(args, bindings, closureContext, (argNames, bindings) => {
            IRLet(name, convertIDToIRType(id), RhsPrim(op, argNames), cont(name, bindings))
          })
        }
      }
      case LetBinding(id, varName, typ, rhs, next) => {
        convertASTToIR(rhs, bindings, closureContext, (resName: String, bindings) => {
          if(varName == resName) {
            convertASTToIR(next, bindings + (varName -> convertType(typ)), closureContext - varName, cont)
          } else {
            IRLet(varName, convertType(typ), RhsPrim("id", List(resName)),
              convertASTToIR(next, bindings + (varName -> convertType(typ)), closureContext - varName, cont)
            )
          }
        }, varName)
      }
      case VarBinding(id, varName, typ, rhs, next) => {
        val valName = generateName("varInit");

        convertASTToIR(rhs, bindings, closureContext, (valName: String, bindings) => {
          IRVar(varName, IRVarPtr(convertType(typ)), RhsPrim("id", List(valName)),
            convertASTToIR(next, bindings + (varName -> IRVarPtr(convertType(typ))), closureContext - varName, cont)
          )
        }, varName)
      }
      case IfStatement(id, cond, thenBr, elseBr) => {
        convertASTToIR(cond, bindings, closureContext, (condName: String, bindings) => {
          val res = generateName("res");
          val finallyCont = generateName("finCont");
          val thenCont = generateName("thenCont");
          val elseCont = generateName("elseCont");
          val argTyp = convertIDToIRType(id);
          IRLet(finallyCont, IRCont(List(argTyp)), RhsDefC(List((res, argTyp)), cont(res, bindings)),
            IRLet(thenCont, IRCont(List()), RhsDefC(List(), convertASTToIR(thenBr, bindings, closureContext, (res, bindings) => IRCallC(finallyCont, List(res)))),
              IRLet(elseCont, IRCont(List()), RhsDefC(List(), convertASTToIR(elseBr, bindings, closureContext, (res, bindings) => IRCallC(finallyCont, List(res)))),
                IRIf(condName, thenCont, elseCont)
              )
            )
          )
        })
      }
      case AccessExp(id, root, label) => {
        val name = generateName("access");
        val typ = convertIDToIRType(id);
        val ptrName = generateName("ptr");
        convertASTToIR(root, bindings, closureContext, (rootName: String, bindings) => {
          if(refReqested) {
            IRAccess(ptrName, IRVarPtr(typ), rootName, label, cont(ptrName, bindings))
          } else {
            IRAccess(ptrName, IRVarPtr(typ), rootName, label,
              IRLet(name, typ, RhsDeref(ptrName), cont(name, bindings)))
          }
        }, "", true);
      }
      case FunctionCall(id, function, args) => {
        function match {
          case AccessExp(accessId, root, label) => {
            val typ = convertIDToIRType(root.getID);
            typ match {
              case IRClass(className) => {
                val classStruct = IRTypes.classMap(className)
                val vmtStruct = IRTypes.vmtMap(className)
                if(classStruct._2.methods.contains(label)) {
                  convertASTToIR(FunctionCall(id, VarName(accessId, className + "$" + label), root :: args), bindings, closureContext, cont)
                } else {
                  val vmtMethod = vmtStruct._2.methods(label);
                  convertASTToIR(root, bindings, closureContext, (rootName, bindings) => {
                    convertList(args, bindings, closureContext, (argsNames, bindings) => {
                      val vmtPtrName = generateName("vmtPtr");
                      val vmtName = generateName("vmt");
                      val funcPtrName = generateName("funcPtr");
                      val funcName = generateName("func");
                      val contName = generateName("cont");
                      val retName = generateName("ret");
                      IRAccess(vmtPtrName, IRVarPtr(IRVmt(className)), rootName, "_vmt",
                        IRLet(vmtName, IRVmt(className), RhsDeref(vmtPtrName),
                          IRAccess(funcPtrName, IRVarPtr(vmtMethod), vmtName, label,
                            IRLet(funcName, vmtMethod, RhsDeref(funcPtrName),
                              IRLet(contName, IRCont(List(vmtMethod.ret)), RhsDefC(List((retName, vmtMethod.ret)), cont(retName, bindings)),
                                IRCallF(funcName, contName, argsNames)
                              )
                            )
                          )
                        )
                      )
                    })
                  })
                }
              }
            }
          }
          case _ => {
            val name = generateName("access");
            convertASTToIR(function, bindings, closureContext, (funName, bindings) => {
              convertList(args, bindings, closureContext, (argsNames, bindings) => {
                val contName = generateName("cont");
                val ret = generateName("ret");
                val argTyp = convertIDToIRType(id);
                IRLet(contName, IRCont(List(argTyp)), RhsDefC(List((ret, argTyp)), {
                  IRLet(name, convertIDToIRType(id), RhsPrim("id", List(ret)), cont(name, bindings))
                }),
                  IRCallF(funName, contName, argsNames)
                )
              })
            })
          }
        }
      }
      case LambdaExpression(id, attrs, args, retType, body) => {
        val name = requestedName; //generateName("lambda");
        val contName = generateName("cont");
        IRLet(name, IRFuncPtr(attrs, args.map(elem => convertType(elem._2)), convertType(retType)), RhsDefF(contName, attrs, args.map(elem => (elem._1, convertType(elem._2))),
          convertASTToIR(body, bindings, closureContext -- args.map((pair) => pair._1), (resName: String, bindings) =>
            IRCallC(contName, List(resName))
          ), convertType(ExpressionDataMap.getType(id) match {
            case FunType(attrs, args, ret) => ret
          })), cont(name, bindings)
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
              IRAccess(fieldPtr, IRVarPtr(typ), ptrName, currPair._1,
                IRSet(fieldPtr, currPair._1, prevCont())
              )
            }
          }
          newCont()
        }

        val funcTyp = convertIDToIRType(id)
        val classTyp = funcTyp match { case IRFuncPtr(attrs, args, ret) => ret };
        val classCont = generateName("cont");

        val allocName = generateName();
        val fieldMap = classTyp match {
          case IRClass(className) => IRTypes.classMap(className)._2.fields
        };

        val vmtPtr = generateName("vmtptr");
        val vmtTyp = classTyp match {
          case IRClass(className) => IRVmt(className)
        }

        val className = requestedName;
        val closureName = generateName("closure");
        var newCont = (bindings: Map[String, IRType]) => {
          println(bindings);
          IRLet(className, funcTyp, RhsDefF(classCont, List(), args.map(elem => (elem._1, convertType(elem._2))),
            IRLet(allocName, classTyp, RhsClassAlloc(classTyp),
              IRAccess(vmtPtr, IRVarPtr(vmtTyp), allocName, "_vmt",
                IRSet(vmtPtr, className + "$_vmt",
                  reduce(args, allocName, () => {
                    IRCallC(classCont, List(allocName))
                  })
                )
              )
            ), classTyp), cont(className, bindings))
        };

        val varId = ExpressionDataMap.cloneID(id);

        ExpressionDataMap.putType(varId, ExpressionDataMap.getType(varId) match {
          case FunType(attrs, args, classTyp@ClassType(className, fields, methods)) => classTyp
        });

        val newClosureCtx = closureContext ++ args.map(
          (argPair) => {
            val accessId = ExpressionDataMap.cloneID(id);

            ExpressionDataMap.putType(accessId, ExpressionDataMap.getType(accessId) match {
              case FunType(attrs, args, classTyp@ClassType(className, fields, methods)) => {
                if(fields.contains(argPair._1)) {
                  fields(argPair._1)
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

          val defF = RhsDefF(contName, pair._2.attrs, (closureName, classTyp) :: lambda.args.map(elem => (elem._1, convertType(elem._2))),
            convertASTToIR(lambda.body, bindings + (closureName -> classTyp), newClosureCtx, (resName: String, bindings) =>
              IRCallC(contName, List(resName))
            ), convertType(lambda.retType))
          val currCont = newCont;
          val currPair = pair;
          val funcTyp = IRFuncPtr(pair._2.attrs, classTyp :: lambda.args.map(elem => convertType(elem._2)), convertType(lambda.retType));
          newCont = (bindings) => IRLet(
            className + "$" + currPair._1,
            funcTyp,
            defF,
            currCont(bindings + ((className + "$" + currPair._1) -> funcTyp))
          )
        }

        newCont(bindings);
      }
    }
  }

}
