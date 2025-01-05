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
case class RhsDefF(cont: String, attrs: List[String], args: List[(String, IRType)], body: IRExp, retTyp: IRType) extends IRRHS {
  override def toString: String = attrs.mkString("|") + "[" + cont +"](" + args.map((pair) => pair._1 + ": " + pair._2).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsDefC(args: List[(String, IRType)], body: IRExp) extends IRRHS {
  override def toString: String = "(" + args.map((pair) => (pair._1 + ": " + pair._2)).mkString(", ") + ") => {\n" + body.toString.split("\n").map(elem => "  " + elem).mkString("\n") + "\n}";
}
case class RhsAlloc(typ: IRType) extends IRRHS {
  override def toString: String = "alloc(" + typ + ")";
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
      case FunType(attrs, args, ret) => IRFuncPtr(attrs, args.map(convertType), convertType(ret))
      case ClassType(className, fields, methods) => {
        IRTypes.vmtMap = IRTypes.vmtMap + (className -> (generateName("struct"), VmtStruct(
          methods.filter(pair => pair._2.attrs.contains("virtual")).map((pair) => pair._1 -> (
            convertType(pair._2) match {
              case funcPtr@IRFuncPtr(attrs, args, retTyp) => IRFuncPtr(attrs, IRClass(className) :: args, retTyp)
            }))
        )))
        IRTypes.classMap = IRTypes.classMap + (className -> (generateName("struct"), ClassStruct(
          fields.map(pair => pair._1 -> convertType(pair._2)),
          methods
            .filter(pair => !pair._2.attrs.contains("virtual"))
            .map((pair) => pair._1 -> ((
              convertType(pair._2) match {
                case funcPtr@IRFuncPtr(attrs, args, retTyp) => IRFuncPtr(attrs, IRClass(className) :: args, retTyp)
              }))),
          IRVmt(className),
        )));
        IRValPtr(IRClass(className))
      }
      case _ => throw new RuntimeException("Unknown type to translate: " + typ);
    }
  }

  private def convertIDToIRType(id: ExpID): IRType = {
    convertType(ExpressionDataMap.getType(id))
  }

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
              case IRValPtr(IRClass(className)) => {
                val classStruct = IRTypes.classMap(className)
                val vmtStruct = IRTypes.vmtMap(className)
                if(classStruct._2.methods.contains(label)) {
                  convertASTToIR(name, FunctionCall(id, VarName(accessId, className + "$" + label), root :: args), closureContext, cont)
                } else {
                  val vmtMethod = vmtStruct._2.methods(label);
                  convertASTToIR(generateName(), root, closureContext, (rootName) => {
                    convertList(args, closureContext, (argsNames) => {
                      val vmtPtrName = generateName("vmtPtr");
                      val vmtName = generateName("vmt");
                      val funcPtrName = generateName("funcPtr");
                      val funcName = generateName("func");
                      val contName = generateName("cont");
                      val retName = generateName("ret");
                      IRLet(vmtPtrName, IRValPtr(IRValPtr(IRVmt(className))), RhsAccess(rootName, "_vmt"),
                        IRLet(vmtName, IRValPtr(IRVmt(className)), RhsDeref(vmtPtrName),
                          IRLet(funcPtrName, IRValPtr(vmtMethod), RhsAccess(vmtName, label),
                            IRLet(funcName, vmtMethod, RhsDeref(funcPtrName),
                              IRLet(contName, IRCont(List(vmtMethod.ret)), RhsDefC(List((retName, vmtMethod.ret)), cont(retName)),
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
      case LambdaExpression(id, attrs, args, retType, body) => {
        val contName = generateName("cont");
        IRLet(name, IRFuncPtr(attrs, args.map(elem => convertType(elem._2)), convertType(retType)), RhsDefF(contName, attrs, args.map(elem => (elem._1, convertType(elem._2))),
          convertASTToIR(generateName(), body, closureContext -- args.map((pair) => pair._1), (resName: String) =>
            IRCallC(contName, List(resName))
          ), convertType(ExpressionDataMap.getType(id) match {
            case FunType(attrs, args, ret) => ret
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

        val funcTyp = convertIDToIRType(id)
        val classTyp = funcTyp match { case IRFuncPtr(attrs, args, ret) => ret };
        val classCont = generateName("cont");

        val allocName = generateName();
        val fieldMap = classTyp match {
          case IRValPtr(IRClass(className)) => IRTypes.classMap(className)._2.fields
        };

        val vmtPtr = generateName("vmtptr");
        val vmtTyp = classTyp match {
          case IRValPtr(IRClass(className)) => IRValPtr(IRVmt(className))
        }

        val className = name;
        val closureName = generateName("closure");
        var newCont = () => {
          IRLet(className, funcTyp, RhsDefF(classCont, List(), args.map(elem => (elem._1, convertType(elem._2))),
            IRLet(allocName, classTyp, RhsAlloc(classTyp match { case IRValPtr(typ) => typ }),
              IRLet(vmtPtr, IRValPtr(vmtTyp), RhsAccess(allocName, "_vmt"),
                IRLet(generateName(), vmtTyp, RhsPrim("=", List(vmtPtr, className + "$_vmt")),
                  reduce(args, allocName, () => {
                    IRCallC(classCont, List(allocName))
                  })
                )
              )
            ), classTyp), cont(name))
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
            convertASTToIR(generateName(), lambda.body, newClosureCtx, (resName: String) =>
              IRCallC(contName, List(resName))
            ), convertType(lambda.retType))
          val currCont = newCont;
          val currPair = pair;
          newCont = () => IRLet(name + "$" + currPair._1, IRFuncPtr(pair._2.attrs, classTyp :: lambda.args.map(elem => convertType(elem._2)), convertType(lambda.retType)), defF, currCont())
        }

        newCont();
      }
    }
  }

}
