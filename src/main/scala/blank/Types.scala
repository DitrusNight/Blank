package blank

import blank.ErrorHandler.raiseError

import scala.collection.immutable.List

abstract class Type
case class UnknownType() extends Type {
  override def toString: String = "unk";
}
case class UnitType() extends Type {
  override def toString: String = "{}";
}
case class BaseType(name: String) extends Type {
  override def toString: String = name;
}
case class TypeVar(name: String) extends Type {
  override def toString: String = name;
}
case class VarClassType(name: String, props: Map[String, Type]) extends Type {
  override def toString: String = "{\n" + props.mkString(", \n") + "\n}";
}
case class ClassType(fields: Map[String, Type], vmt: Map[String, FunType], methods: Map[String, FunType]) extends Type {
  override def toString: String = "{\n  _vmt: {" + vmt.mkString(", \n") + "}, " + fields.mkString(", \n") + ", " + methods.mkString(", \n") + "\n}";
}
case class FunType(args: List[Type], ret: Type) extends Type {
  override def toString: String = "(" + args.mkString(", ") + ") => " + ret;
}

class Types {

  var constraints: Map[String, Type] = Map();

  private val subtypingRelations = List(
    (BaseType("u8"), BaseType("u16")),
    (BaseType("u16"), BaseType("u32")),
    (BaseType("u32"), BaseType("u64")),
    (BaseType("i8"), BaseType("i16")),
    (BaseType("i16"), BaseType("i32")),
    (BaseType("i32"), BaseType("i64")),
    (BaseType("u8"), BaseType("i16")),
    (BaseType("u16"), BaseType("i32")),
    (BaseType("u32"), BaseType("i64")),
    (BaseType("f32"), BaseType("f64")),
    (BaseType("u16"), BaseType("f32")),
    (BaseType("u32"), BaseType("f64")),
    (BaseType("i16"), BaseType("f32")),
    (BaseType("i32"), BaseType("f64")),

    (BaseType("u8"), BaseType("i32")),
  )

  private var arithmeticMap: Map[(Type, Type), Type] = Map();

  private val primNums = List(
    BaseType("u8"), BaseType("u16"), BaseType("u32"), BaseType("u64"),
    BaseType("i8"), BaseType("i16"), BaseType("i32"), BaseType("i64"),
    BaseType("f32"), BaseType("f64"),
  )

  private def getSubtypes(types: Set[Type]): Set[Type] = {
    var newSet: Set[Type] = Set() ++ types;
    for(typ <- types) {
      for(subtype <- subtypingRelations) {
        if(subtype._2 == typ) {
          newSet = newSet + subtype._1;
        }
      }
    }
    if(newSet.size == types.size)
      newSet
    else getSubtypes(newSet)
  }

  def populateArithmeticTypes(): Unit = {
    for(primNum <- primNums) {
      val subtypes = getSubtypes(Set(primNum))
      for(subtype <- subtypes) {
        arithmeticMap = arithmeticMap + ((subtype, primNum) -> primNum);
        arithmeticMap = arithmeticMap + ((primNum, subtype) -> primNum);
      }
    }
  }

  def followConstraintsFun(typ: FunType, path: Set[String]): FunType = {
    FunType(
      typ.args.map((elem) => followConstraints(elem, path)),
      followConstraints(typ.ret, path)
    )
  }

  def followConstraints(typ: Type, path: Set[String] = Set()): Type = {
    typ match {
      case TypeVar(name) =>
        if(path.contains(name))
          throw new RuntimeException("Recursive type definition. " + constraints);
        constraints.get(name) match {
          case Some(newTyp) => followConstraints(newTyp, path + name)
          case _ => typ
        }
      case ClassType(fields, vmt, methods) =>
        ClassType(
          fields.map(pair => pair._1 -> followConstraints(pair._2, path)),
          vmt.map(pair => pair._1 -> followConstraintsFun(pair._2, path)),
          methods.map(pair => pair._1 -> followConstraintsFun(pair._2, path))
        )
      case VarClassType(label, props) =>
        VarClassType(label,
          props.map(pair => pair._1 -> followConstraints(pair._2, path))
        )
      case FunType(args, ret) => {
        followConstraintsFun(FunType(args, ret), path)
      }
      case _ => typ
    }
  }

  def unionType(inferredType: Type, expTyp: Type, exp: Option[Expression] = None): Type = {
    if(inferredType == expTyp)
      return inferredType;
    val newInfType = followConstraints(inferredType);
    val newExpType = followConstraints(expTyp);
    (newInfType, newExpType) match {
      case (TypeVar(x), TypeVar(y)) => {
        if(x != y)
          constraints = constraints + (x -> (newExpType));
        newExpType
      }
      case (TypeVar(x), _) => {
        constraints = constraints + (x -> (newExpType));
        newExpType
      }
      case (_, TypeVar(y)) => {
        constraints = constraints + (y -> (newInfType));
        newInfType
      }
      case (VarClassType(varName1, inferProps), VarClassType(varName2, expProps)) => {
        // TODO Figure out union of these. IDK if this is right.
        var newProps: Map[String, Type] = Map()
        for(label <- expProps.keySet) {
          if(inferProps.contains(label)) {
            newProps = newProps + (label -> unionType(inferProps(label), expProps(label), exp))
          } else {
            newProps = newProps + (label -> expProps(label))
          }
        }
        for(label <- inferProps.keySet) {
          if(!expProps.contains(label)) {
            newProps = newProps + (label -> inferProps(label))
          }
        }
        val newVarClass = VarClassType(varName1, newProps)
        constraints = constraints + (varName1 -> newVarClass) + (varName2 -> TypeVar(varName1))
        VarClassType(varName1, newProps)
      }
      case (VarClassType(varName1, props), ClassType(fields, vmt, methods)) => {
        // TODO Better error handling.
        val classProps = fields ++ vmt ++ methods;
        for(label <- props.keySet) {
          if(classProps.contains(label)) {
            unionType(props(label), fields(label), exp)
          } else {
            throw new RuntimeException("Unable to union class types. No field " + label + " found.");
          }
        }
        newExpType
      }
      case (ClassType(fields, vmt, methods), VarClassType(varName, props)) => {
        // TODO Better error handling.
        val newProps = fields ++ vmt ++ methods
        for(label <- props.keySet) {
          if(!newProps.contains(label)) {
            throw new RuntimeException("Unable to union class types. No field " + label + " found.");
          }
        }
        constraints = constraints + (varName -> newExpType)
        newExpType
      }
      case (FunType(args1, ret1), FunType(args2, ret2)) => {
        // TODO Better error handling.
        // Note: Arguments must be contravariant.
        val newArgs = args1.zip(args2).map(pair => unionType(pair._2, pair._1, exp))
        // Return type must be covariant.
        val newRet = unionType(ret1, ret2);
        FunType(newArgs, newRet)
      }
      case (_, _) => {
        if(!getSubtypes(Set(newExpType)).contains(newInfType)) {
          exp match {
            case Some(exp) =>
              raiseError(exp.getID, "Types do not align. " + newInfType + " !<: " + newExpType)
            case None =>
              throw new RuntimeException("Types do not align. " + newInfType + " !<: " + newExpType)
          }
        }
        newInfType
      }
    }
  }

  def inferType(bindings: Map[String, (Boolean, Type)], exp: Expression): Type = {
    exp match {
      case IntLit(id, lit: Int) => {
        if(lit < 0)
          BaseType("i64");
        else if(lit < 255)
          BaseType("u8");
        else if(lit < 65535)
          BaseType("u16");
        else
          BaseType("u32");
      }
      case FloatLit(id, lit: Float) => {
        BaseType("f64")
      }
      case UnitLit(id) => UnitType()
      case VarName(id, name: String) => bindings.get(name) match {
        case Some((mut: Boolean, typ: Type)) => typ
        case None => raiseError(id, "Unknown variable name"); UnitType()
      }
      case PrimOp(id, op: String, args: List[Expression]) => {
        val argTypes = args.map(arg => inferType(bindings, arg));
        op match {
          case "id" => argTypes(0)
          case "-" | "+" | "*" | "/" => {
            // TODO: Include type vars.
            (argTypes(0), argTypes(1)) match {
              case (TypeVar(x), _) => {
                unionType(argTypes(0), BaseType("i64"), Some(exp));
                BaseType("i64")
              }
              case (_, TypeVar(y)) => {
                unionType(BaseType("i64"), argTypes(1), Some(exp));
                BaseType("i64")
              }
              case (_, _) => {
                arithmeticMap.get((argTypes(0), argTypes(1))) match {
                  case Some(resType: Type) => resType
                  case _ =>
                    raiseError(id, "Unable to conform arithmetic arguments to numbers"); UnitType()
                }
              }
            }
          }
          case ">" | "<" => {
            // TODO: Include type vars.
            (argTypes(0), argTypes(1)) match {
              case (TypeVar(x), _) => {
                unionType(argTypes(0), argTypes(1), Some(exp))
                BaseType("boolean")
              }
              case (_, TypeVar(y)) => {
                unionType(argTypes(0), argTypes(1), Some(exp))
                BaseType("boolean")
              }
              case (_, _) => {
                arithmeticMap.get((argTypes(0), argTypes(1))) match {
                  case Some(resType: Type) => BaseType("boolean")
                  case _ =>
                    raiseError(id, "Unable to conform arithmetic arguments to numbers"); UnitType()
                }
              }
            }
          }
          case "=" => {
            args.head match {
              case VarName(id2, label) => {
                if(!bindings(label)._1) {
                  raiseError(id2, "Unable to set non-modifiable binding: " + label);
                }
              }
              case _ => raiseError(id, "Unable to set expression to value.");
            }
            (argTypes(0), argTypes(1)) match {
              case (TypeVar(x), _) => {
                unionType(argTypes(0), argTypes(1), Some(exp))
              }
              case (_, TypeVar(y)) => {
                unionType(argTypes(0), argTypes(1), Some(exp))
              }
              case (_, _) => {
                arithmeticMap.get((argTypes(0), argTypes(1))) match {
                  case Some(resType: Type) => resType
                  case _ =>
                    raiseError(id, "Unable to conform arithmetic arguments to numbers"); UnitType()
                }
              }
            }
          }
          case _ => raiseError(id, "Unknown primitive " + op); UnitType()
        }
      }
      case LetBinding(id, varName: String, typ: Type, rhs: Expression, next: Expression) => {
        val inferredType = inferType(bindings, rhs);
        val resType = unionType(inferredType, typ, Some(exp));
        inferType(bindings + (varName -> (false, resType)), next)
      }
      case VarBinding(id, varName: String, typ: Type, rhs: Expression, next: Expression) => {
        val inferredType = inferType(bindings, rhs);
        val resType = unionType(inferredType, typ, Some(exp));
        inferType(bindings + (varName -> (true, resType)), next)
      }
      case IfStatement(id, cond: Expression, thenBr: Expression, elseBr: Expression) => {
        unionType(inferType(bindings, cond), BaseType("boolean"), Some(exp))
        unionType(inferType(bindings, thenBr), inferType(bindings, elseBr), Some(exp))
      }
      case AccessExp(id, root: Expression, label: String) => {
        inferType(bindings, root) match {
          case ClassType(fields, vmt, methods) =>
            val props = fields ++ vmt ++ methods;
            props.get(label) match {
              case Some(typ) => typ
              case None =>
                raiseError(id, "Unable to access label " + label); UnitType()
            }
          case TypeVar(name) =>
            val typeVar = TypeVar("?" + uniqInd())
            constraints = constraints + (name -> VarClassType(name, Map(label -> typeVar)));
            typeVar
          case VarClassType(name, props) =>
            val typeVar = TypeVar("?" + uniqInd())
            val newProps = props + (label -> typeVar);
            constraints = constraints + (name -> VarClassType(name, newProps))
            typeVar
          case _ =>
            raiseError(id, "Unable to access label " + label); UnitType()
        }
      }
      case FunctionCall(id, function: Expression, args: List[Expression]) => {
        val argTypes = args.map(exp => inferType(bindings, exp))
        val retType = TypeVar("?" + uniqInd());
        val funType = inferType(bindings, function)
        unionType(funType, FunType(argTypes, retType), Some(exp)) match {
          case FunType(args, res) => res
        }
      }
      case LambdaExpression(id, args: List[(String, Type)], retType: Type, body: Expression) => {
        val bodyBindings = bindings ++ args.map((pair) => pair._1 -> (false, pair._2));
        val inferredType = inferType(bodyBindings, body);
        val bodyType = unionType(inferredType, retType, Some(exp));
        val argTypes = args.map((pair) => pair._2);
        FunType(argTypes, bodyType)
      }
      case ClassExpression(id, args: List[(String, Type)], body: Expression) => {
        var runningBindings = bindings ++ args.map((pair) => pair._1 -> (false, pair._2));
        val inferredType = inferType(runningBindings, body);
        val argTypes = args.map((pair) => pair._2);
        var fields: Map[String, Type] = Map()
        val vmt: Map[String, FunType] = Map()
        var methods: Map[String, FunType] = Map()
        var exp: Option[Expression] = Some(body);
        while(exp.isDefined) {
          exp match {
            case Some(LetBinding(id, varName, typ, rhs@LambdaExpression(id2, args, retType, body), next)) => {
              val typ = inferType(runningBindings, rhs);
              typ match {
                case funType@FunType(_, _) => methods = methods + (varName -> funType);
              }
              runningBindings = runningBindings + (varName -> (false, typ));
              exp = Some(next);
            }
            case Some(LetBinding(id, varName, typ, rhs, next)) => {
              val typ = inferType(runningBindings, rhs);
              fields = fields + (varName -> typ);
              runningBindings = runningBindings + (varName -> (false, typ));
              exp = Some(next);
            }
            case Some(VarBinding(id, varName, typ, rhs, next)) => {
              val typ = inferType(runningBindings, rhs);
              fields = fields + (varName -> typ);
              runningBindings = runningBindings + (varName -> (true, typ))
              exp = Some(next);
            }
            case _ => exp = None;
          }
        }
        FunType(argTypes, ClassType(fields, vmt, methods))
      }
    }
  }

  def convertTypes(bindings: Map[String, (Boolean, Type)], exp: Expression, cont: (Expression) => Expression): Expression = {
    exp match {
      case IntLit(id, lit: Int) => {
        ExpressionDataMap.putType(exp, inferType(bindings, exp));
        cont(exp)
      }
      case FloatLit(id, lit: Float) => {
        ExpressionDataMap.putType(exp, inferType(bindings, exp));
        cont(exp)
      }
      case UnitLit(id) => {
        ExpressionDataMap.putType(exp, UnitType());
        cont(exp)
      }
      case VarName(id, name: String) => {
        ExpressionDataMap.putType(exp, bindings(name)._2);
        cont(exp)
      }
      case PrimOp(id, op: String, args: List[Expression]) => {
        def convertList(list: List[Expression], cont: (List[Expression]) => Expression): Expression = {
          var newCont = cont;
          for (arg <- list.reverse) {
            val prevCont = newCont;
            newCont = (exps: List[Expression]) => {
              convertTypes(bindings, arg, (newExp) => {
                prevCont(newExp :: exps)
              })
            }
          }
          newCont(List())
        }
        convertList(args, (newArgs) => {
          ExpressionDataMap.putType(id, inferType(bindings, exp));
          cont(PrimOp(id, op, newArgs))
        })
      }
      case LetBinding(id, varName: String, typ: Type, rhs: Expression, next: Expression) => {
        val newType = followConstraints(typ);
        convertTypes(bindings, rhs, (newRhs) => {
          ExpressionDataMap.putType(newRhs, newType);
          convertTypes(bindings + (varName -> (false, newType)), next, (newNext) => {
            ExpressionDataMap.putType(id, ExpressionDataMap.getType(newNext));
            cont(LetBinding(id, varName, newType, newRhs, newNext))
          })
        })
      }
      case VarBinding(id, varName: String, typ: Type, rhs: Expression, next: Expression) => {
        val newType = followConstraints(typ);
        convertTypes(bindings, rhs, (newRhs) => {
          ExpressionDataMap.putType(newRhs, newType);
          convertTypes(bindings + (varName -> (true, newType)), next, (newNext) => {
            ExpressionDataMap.putType(id, ExpressionDataMap.getType(newNext));
            cont(VarBinding(id, varName, newType, newRhs, newNext))
          })
        })
      }
      case IfStatement(id, cond: Expression, thenBr: Expression, elseBr: Expression) => {
        convertTypes(bindings, cond, (newCond) => {
          ExpressionDataMap.putType(newCond, BaseType("boolean"));
          convertTypes(bindings, thenBr, (newThenBr) => {
            convertTypes(bindings, elseBr, (newElseBr) => {
              ExpressionDataMap.putType(id, inferType(bindings, exp));
              cont(IfStatement(id, newCond, newThenBr, newElseBr))
            })
          })
        })
      }
      case AccessExp(id, root: Expression, label: String) => {
        convertTypes(bindings, root, (newRoot) => {
          ExpressionDataMap.putType(id, inferType(bindings, exp));
          cont(AccessExp(id, newRoot, label))
        })
      }
      case FunctionCall(id, function: Expression, args: List[Expression]) => {
        def convertList(list: List[Expression], cont: (List[Expression]) => Expression): Expression = {
          var newCont = cont;
          for (arg <- list.reverse) {
            val prevCont = newCont;
            newCont = (exps: List[Expression]) => {
              convertTypes(bindings, arg, (newExp) => {
                prevCont(newExp :: exps)
              })
            }
          }
          newCont(List())
        }

        convertList(args, (newArgs) => {
          convertTypes(bindings, function, (newFunction) => {
            ExpressionDataMap.putType(id, inferType(bindings, exp));
            cont(FunctionCall(id, newFunction, newArgs))
          });
        })
      }
      case LambdaExpression(id, args: List[(String, Type)], retType: Type, body: Expression) => {
        convertTypes(bindings ++ args.map((pair) => pair._1 -> (false, pair._2)), body, (newBody) => {
          ExpressionDataMap.putType(id, inferType(bindings, exp));
          cont(LambdaExpression(id, args.map((pair) => pair._1 -> followConstraints(pair._2)), followConstraints(retType), newBody))
        });
      }
      case ClassExpression(id, args: List[(String, Type)], body: Expression) => {
        convertTypes(bindings ++ args.map((pair) => pair._1 -> (false, pair._2)), body, (newBody) => {
          ExpressionDataMap.putType(id, inferType(bindings, exp));
          cont(ClassExpression(id, args.map((pair) => pair._1 -> followConstraints(pair._2)), newBody))
        });
      }
    }
  }
}
