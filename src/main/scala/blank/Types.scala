package blank

import scala.collection.immutable.List

abstract class Type
case class UnitType() extends Type {
  override def toString: String = "{}";
}
case class BaseType(name: String) extends Type {
  override def toString: String = name;
}
case class TypeVar(name: String) extends Type {
  override def toString: String = name;
}
case class ClassType(props: Map[String, Type]) extends Type {
  override def toString: String = "{\n" + props.mkString(", \n") + "\n}";
}
case class FunType(args: List[Type], ret: Type) extends Type {
  override def toString: String = "(" + args.mkString(", ") + ") => " + ret;
}

class Types {

  var constraints: Map[TypeVar, Type] = Map();
  private var uniqueIndex = 0;
  private def uniqInd: Int = {
    uniqueIndex += 1;
    uniqueIndex
  }

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
    (BaseType("i32"), BaseType("f64"))
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

  def typeConforms(inferredType: Type, expTyp: Type): Type = {
    (inferredType, expTyp) match {
      case (TypeVar(x), TypeVar(y)) => {
        constraints = constraints + (TypeVar(x) -> (expTyp));
        expTyp
      }
      case (TypeVar(x), _) => {
        constraints = constraints + (TypeVar(x) -> (expTyp));
        expTyp
      }
      case (_, TypeVar(y)) => {
        constraints = constraints + (TypeVar(y) -> (inferredType));
        inferredType
      }
      case (FunType(args1, ret1), FunType(args2, ret2)) => {
        // TODO Better error handling.
        // Note: Arguments must be contravariant.
        args1.zip(args2).forall(pair => typeConforms(pair._2, pair._1) != BaseType(""))
        // Return type must be covariant.
        typeConforms(ret2, ret1);
        expTyp
      }
      case (_, _) => {

        if(!getSubtypes(Set(expTyp)).contains(inferredType))
            throw new RuntimeException("Types do not align. " + expTyp + " <: " + inferredType)
        inferredType
      }
    }
  }

  def inferType(bindings: Map[String, Type], exp: Expression): Type = {
    exp match {
      case IntLit(lit: Int) => {
        if(lit < 0)
          BaseType("i64");
        else if(lit < 255)
          BaseType("u8");
        else if(lit < 65535)
          BaseType("u16");
        else
          BaseType("u32");
      }
      case FloatLit(lit: Float) => {
        BaseType("f64")
      }
      case UnitLit() => UnitType()
      case VarName(name: String) => bindings.get(name) match {
        case Some(typ: Type) => typ
        // TODO Better errors
        case None => throw new RuntimeException("Unknown variable name: " + name);
      }
      case PrimOp(op: String, args: List[Expression]) => {
        val argTypes = args.map(arg => inferType(bindings, arg));
        op match {
          case "id" => argTypes(0)
          case "-" | "+" | "*" | "/" => {
            // TODO: Include type vars.
            arithmeticMap.get((argTypes(0), argTypes(1))) match {
              case Some(resType: Type) => resType
              case _ =>
                throw new RuntimeException("Unable to conform arithmetic arguments to numbers.")
            }
          }
          case _ => throw new RuntimeException("Unknown primitive " + op)
        }
      }
      case LetBinding(varName: String, typ: Type, rhs: Expression, next: Expression) => {
        val inferredType = inferType(bindings, rhs);
        val resType = typeConforms(inferredType, typ);
        inferType(bindings + (varName -> resType), next)
      }
      case AccessExp(root: Expression, label: String) => {
        inferType(bindings, root) match {
          case ClassType(props) => props.get(label) match {
            case Some(typ) => typ
            case None =>
              throw new RuntimeException("Unable to add access label " + label + " on " + root)
          }
        }
      }
      case FunctionCall(function: Expression, args: List[Expression]) => {
        // TODO
        UnitType()
      }
      case LambdaExpression(args: List[(String, Type)], retType: Type, body: Expression) => {
        val bodyBindings = bindings ++ args.map((pair) => pair._1 -> pair._2);
        val inferredType = inferType(bodyBindings, body);
        val bodyType = typeConforms(inferredType, retType);
        val argTypes = args.map((pair) => pair._2);
        FunType(argTypes, bodyType)
      }
    }
  }

}
