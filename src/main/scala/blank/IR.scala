package blank

import scala.collection.immutable.{AbstractSet, SortedSet}

case class IRFunction(cont: String, args: List[(String, IRType)], body: IRExp, retTyp: IRType) {
  override def toString: String = "[" + cont + "](" + args.map((pair) => pair._1 + ": " + pair._2).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}

class IRProgram {
  var methods: Map[String, IRFunction] = Map();
  var globals: Map[String, IRRHS] = Map();

  def addMethod(name: String, method: IRFunction) = {
    methods = methods + (name -> method);
  }

  def addGlobal(name: String, value: IRRHS) = {
    globals = globals + (name -> value);
  }

  override def toString: String = {
    globals.mkString(",\n")
     + "\n"
     + methods.mkString(",\n")
  }

}

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
case class RhsDefC(args: List[(String, IRType)], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.map((pair) => (pair._1 + ": " + pair._2)).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsFuncAlloc(func: IRFuncPtr, map: Map[String, IRType]) extends IRRHS {
  override def toString: String = "alloc(" + func + ", " + map + ")";
}
case class RhsClassAlloc(typ: IRType) extends IRRHS {
  override def toString: String = "alloc(" + typ + ")";
}

type ClosureContext = Map[String, ((String, Map[String, IRType]) => IRExp) => IRExp];

abstract class PtrProps;
case class PtrHeap() extends PtrProps;
case class PtrStack() extends PtrProps;
case class PtrValue() extends PtrProps;

object IR {

  def generateName(prefix: String = "name"): String = prefix + uniqInd();

  private def convertList(list: List[Expression], cont: (List[String], Map[String, IRType]) => IRExp)(implicit bindings: Map[String, IRType], closureContext: ClosureContext, program: IRProgram): IRExp = {
    var newCont = cont;
    for (arg <- list.reverse) {
      val prevCont = newCont;
      newCont = (elemNames: List[String], bindings) => {
        convertASTToIR(arg, (elemName, bindings) => {
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
      case FunType(attrs, args, ret) => IRFuncPtr(args.map(convertType), convertType(ret))
      case ClassType(className, fields, methods) => {
        IRTypes.vmtMap = IRTypes.vmtMap + (className -> (generateName("vmt"), VmtStruct(
          methods.filter(pair => pair._2.attrs.contains("virtual")).map((pair) => {pair._1 -> (

            convertType(pair._2) match {
              case funcPtr@IRFuncPtr(args, retTyp) => IRFuncPtr(IRClass(className) :: args, retTyp)
            })})
        )))
        IRTypes.classMap = IRTypes.classMap + (className -> (generateName("class"), ClassStruct(
          fields.map(pair => pair._1 -> convertType(pair._2)),
          methods
            .filter(pair => !pair._2.attrs.contains("virtual"))
            .map((pair) => pair._1 -> ((
              convertType(pair._2) match {
                case funcPtr@IRFuncPtr(args, retTyp) => IRFuncPtr(IRClass(className) :: args, retTyp)
              }))),
          IRVmt(className),
        )));
        IRClass(className)
      }
      case _ => throw new RuntimeException("Unknown type to translate: " + typ);
      //case _ => IRUnit() // TODO
    }
  }

  private def convertIDToIRType(id: ExpID): IRType = {
    convertType(ExpressionDataMap.getType(id))
  }

  def getGlobalBindings(exp: Expression): Map[String, IRType] = {
    exp match {
      case UnitLit(id) => Map()
      case IntLit(id, lit) => Map()
      case FloatLit(id, lit) => Map()
      case VarName(id, name) => Map()
      case PrimOp(id, op, args) => Map()
      case AccessExp(id, root, label) => Map()
      case FunctionCall(id, function, args) => Map()
      case LambdaExpression(id, attrs, args, retType, body) => Map()
      case ClassExpression(id, name, args, methods) => {
        Map(
          (name + "$_vmt", IRVmt(name))
        ) ++ methods.filter((methodMapping) => !methodMapping._2.attrs.contains("virtual"))
          .map((methodMapping) => (
            (name + "$" + methodMapping._1) -> IRFuncPtr(IRClass(name) :: methodMapping._2.args.map((pair) => convertType(pair._2)), convertType(methodMapping._2.retType))
          ))
      }
      case VarBinding(id, varName, typ, rhs, next) => getGlobalBindings(rhs) ++ getGlobalBindings(next)
      case LetBinding(id, varName, typ, rhs, next) => getGlobalBindings(rhs) ++ getGlobalBindings(next)
      case IfStatement(id, cond, thenBr, elseBr) => Map()
    }
  }

  def convertTopLevel(
    exp: Expression,
  )(implicit bindings: Map[String, IRType], closureContext: ClosureContext, program: IRProgram): Unit = {
    exp match {
      case UnitLit(id) => ()
      case IntLit(id, lit) => ()
      case FloatLit(id, lit) => ()
      case VarName(id, name) => ()
      case PrimOp(id, op, args) => ()
      case AccessExp(id, root, label) => ()
      case FunctionCall(id, function, args) => ()
      case IfStatement(id, cond, thenBr, elseBr) => ()
      case LetBinding(id, varName, typ, rhs, next) => {
        rhs match {
          case LambdaExpression(id, attrs, args, retType, body) => {
            val contName = generateName("cont");
            val argsTypes = args.map((pair) => pair._1 -> convertType(pair._2));
            val newRetType = convertType(retType);
            val newBindings = bindings ++ argsTypes;
            val irBody = convertASTToIR(body, (resName, bindings) => IRCallC(contName, List(resName)))(newBindings);
            program.addMethod(varName, IRFunction(contName, argsTypes, irBody, newRetType))

            convertTopLevel(next)(bindings + (varName -> IRFuncPtr(argsTypes.map((pair) => pair._2), newRetType)))
          }
          case _ => convertTopLevel(next)(bindings)
        }
      }
    }
  }

    def convertASTToIR(
    exp: Expression,
    cont: (String, Map[String, IRType]) => IRExp,
    requestedName: String = "",
    refReqested: Boolean = false
  )(implicit bindings: Map[String, IRType], closureCtx: ClosureContext, program: IRProgram): IRExp = {
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
        closureCtx.get(oldName) match {
          case Some(value) => {
            value(cont)
          }
          case None => {
            bindings.get(oldName) match {
              case Some(IRVarPtr(typ)) => {
                if (refReqested) {
                  cont(oldName, bindings)
                } else {
                  val name = generateName("deref")
                  IRLet(name, typ, RhsDeref(oldName), cont(name, bindings))
                }
              }
              case Some(typ) => cont(oldName, bindings)
              case None => {
                println(bindings)
                ErrorHandler.raiseError(id, "Unknown variable: " + oldName);
                cont("", bindings)
              }
            }
          }
        }

      }
      case PrimOp(id, op, args) => {
        val name = generateName("prim");
        if (op == "=") {
          convertASTToIR(args(0), (refName, bindings) => {
            convertASTToIR(args(1), (valueName, bindings) => {
              IRSet(refName, valueName,
                cont(valueName, bindings)
              )
            })
          }, "", true)
        } else {
          convertList(args, (argNames, bindings) => {
            IRLet(name, convertIDToIRType(id), RhsPrim(op, argNames), cont(name, bindings))
          })
        }
      }
      case LetBinding(id, varName, typ, rhs, next) => {
        convertASTToIR(rhs, (resName: String, bindings) => {
          if(varName == resName) {
            convertASTToIR(next, cont)(bindings + (varName -> convertType(typ)))
          } else {
            IRLet(varName, convertType(typ), RhsPrim("id", List(resName)),
              convertASTToIR(next, cont)(bindings + (varName -> convertType(typ)))
            )
          }
        }, varName)
      }
      case VarBinding(id, varName, typ, rhs, next) => {
        val valName = generateName("varInit");

        convertASTToIR(rhs, (valName: String, bindings) => {
          IRVar(varName, IRVarPtr(convertType(typ)), RhsPrim("id", List(valName)),
            convertASTToIR(next, cont)(bindings + (varName -> IRVarPtr(convertType(typ))))
          )
        }, varName)
      }
      case IfStatement(id, cond, thenBr, elseBr) => {
        convertASTToIR(cond, (condName: String, bindings) => {
          val res = generateName("res");
          val finallyCont = generateName("finCont");
          val thenCont = generateName("thenCont");
          val elseCont = generateName("elseCont");
          val argTyp = convertIDToIRType(id);
          IRLet(finallyCont, IRCont(List(argTyp)), RhsDefC(List((res, argTyp)), cont(res, bindings)),
            IRLet(thenCont, IRCont(List()), RhsDefC(List(), convertASTToIR(thenBr, (res, bindings) => IRCallC(finallyCont, List(res)))),
              IRLet(elseCont, IRCont(List()), RhsDefC(List(), convertASTToIR(elseBr, (res, bindings) => IRCallC(finallyCont, List(res)))),
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
        convertASTToIR(root, (rootName: String, bindings) => {
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
                  convertASTToIR(FunctionCall(id, VarName(accessId, className + "$" + label), root :: args), cont)
                } else {
                  val vmtMethod = vmtStruct._2.methods(label);
                  convertASTToIR(root, (rootName, bindings) => {
                    convertList(args, (argsNames, bindings) => {
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
            convertASTToIR(function, (funName, bindings) => {
              val baseFuncType = IRFuncPtr(List(), IRUnit())
              val accessName = generateName("funcPtr")
              val valName = generateName("funcName")
              IRAccess(accessName, IRVarPtr(baseFuncType), funName, "_func",
                IRLet(valName, baseFuncType, RhsDeref(accessName),
                  convertList(args, (argsNames, bindings) => {
                    val contName = generateName("cont");
                    val ret = generateName("ret");
                    val argTyp = convertIDToIRType(id);
                    IRLet(contName, IRCont(List(argTyp)), RhsDefC(List((ret, argTyp)), {
                      IRLet(name, convertIDToIRType(id), RhsPrim("id", List(ret)), cont(name, bindings))
                    }),
                      IRCallF(valName, contName, funName :: argsNames)
                    )
                  })
                )
              )
            })
          }
        }
      }
      case LambdaExpression(id, attrs, args, retType, body) => {
        val name = requestedName; //generateName("lambda");
        val funcTrueName = generateName("lambda");
        val contName = generateName("cont");
        val wrappedFuncName = generateName("wrapped_func");

        val freeVariables = ASTHelper.freeVariables(exp) - name;

        val wrappedFuncTyp = IRWrappedFunc(wrappedFuncName);
        val funcTyp = IRFuncPtr(wrappedFuncTyp :: args.map((pair) => convertType(pair._2)), convertType(retType))
        IRTypes.funcMap = IRTypes.funcMap + (wrappedFuncName -> FuncStruct(funcTyp, freeVariables.map(freeVar => freeVar -> bindings(freeVar)).toMap));
        val funcPtrName = generateName("funcPtr");

        def newCont(runningFreeVars: Set[String], closureCtx: ClosureContext): IRExp = {
          if(runningFreeVars.isEmpty) {
            val newBindings = bindings ++ args.map((pair) => pair._1 -> convertType(pair._2))

            program.addMethod(funcTrueName, IRFunction(contName, ("_wrapper", wrappedFuncTyp) :: args.map(elem => (elem._1, convertType(elem._2))),
              convertASTToIR(body, (resName: String, bindings) =>
                IRCallC(contName, List(resName))
              )(newBindings, closureCtx), convertType(retType)
            ))

            cont(name, bindings + (name -> wrappedFuncTyp))
          } else {
            val head = runningFreeVars.head;
            val typ = bindings(head)
            val closureVarName = generateName("closureVar");
            IRAccess(closureVarName, IRVarPtr(typ), name, head,
              IRSet(closureVarName, head,
                newCont(runningFreeVars.tail, closureCtx + ({
                  (head -> ((cont: (String, Map[String, IRType]) => IRExp) => {
                    val accessName = generateName("access");
                    val varName = generateName("val")
                    IRAccess(accessName, IRVarPtr(typ), "_wrapper", head,
                      IRLet(varName, typ, RhsDeref(accessName),
                        cont(varName, bindings)
                      )
                    )
                  }))
                }))
              )
            )
          }
        }

        IRLet(name, wrappedFuncTyp,  RhsFuncAlloc(funcTyp, freeVariables.map((freeVar) => freeVar -> bindings(freeVar)).toMap),
          IRAccess(funcPtrName, IRVarPtr(funcTyp), name, "_func",
            IRSet(funcPtrName, funcTrueName,
              newCont(freeVariables, closureCtx)
            )
          )
        )
      }
      case ClassExpression(id, className, args, map) => {

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
        val classTyp = funcTyp match { case IRFuncPtr(args, ret) => ret };
        val classCont = generateName("cont");

        val allocName = generateName();
        val fieldMap = IRTypes.classMap(className)._2.fields

        val vmtPtr = generateName("vmtptr");
        val vmtTyp = IRVmt(className)

        val closureName = generateName("closure");
        var newCont = (bindings: Map[String, IRType]) => {
          program.addMethod(className, IRFunction(classCont, args.map(elem => (elem._1, convertType(elem._2))),
            IRLet(allocName, classTyp, RhsClassAlloc(classTyp),
              IRAccess(vmtPtr, IRVarPtr(vmtTyp), allocName, "_vmt",
                IRSet(vmtPtr, className + "$_vmt",
                  reduce(args, allocName, () => {
                    IRCallC(classCont, List(allocName))
                  })
                )
              )
            ), classTyp));
          cont(className, bindings)
        };

        val newClosureCtx: ClosureContext = args.map(
          (argPair) => {
            val accessName = generateName();
            val varName = generateName();
            val typ = convertType(argPair._2);
            argPair._1 -> ((cont: (String, Map[String, IRType]) => IRExp) => {
              IRAccess(accessName, IRVarPtr(typ), closureName, argPair._1,
                IRLet(varName, typ, RhsDeref(accessName), cont(varName, bindings))
              )
            })
          }
        ).toMap ++ map.map(
          (methodMapping) => {
            val accessName = generateName();
            val varName = generateName();
            val typ = convertType(ExpressionDataMap.getType(methodMapping._2.getID));
            methodMapping._1 -> ((cont: (String, Map[String, IRType]) => IRExp) => {
              IRAccess(accessName, IRVarPtr(typ), closureName, methodMapping._1,
                IRLet(varName, typ, RhsDeref(accessName), cont(varName, bindings))
              )
            })
          }
        ) + ("this" -> ((cont: (String, Map[String, IRType]) => IRExp) => cont(closureName, bindings)));

        for(pair <- map) {
          val contName = generateName("cont");
          val lambda = pair._2;

          val functionDef = IRFunction(contName, (closureName, classTyp) :: lambda.args.map(elem => (elem._1, convertType(elem._2))),
            convertASTToIR(lambda.body, (resName: String, bindings) =>
              IRCallC(contName, List(resName))
            )(bindings + (closureName -> classTyp), newClosureCtx), convertType(lambda.retType))
          val currCont = newCont;
          val currPair = pair;
          val funcTyp = IRFuncPtr(classTyp :: lambda.args.map(elem => convertType(elem._2)), convertType(lambda.retType));
          program.addMethod(className + "$" + currPair._1, functionDef);
          newCont = (bindings) => currCont(bindings + ((className + "$" + currPair._1) -> funcTyp))
        }

        newCont(bindings);
      }
    }
  }

}
