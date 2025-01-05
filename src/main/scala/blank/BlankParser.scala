package blank

import blank.ErrorHandler.{combineData, raiseError}

import scala.collection.immutable.List as next
import scala.util.boundary
import scala.util.boundary.break



abstract class Expression(id: ExpID) {
  def getID: ExpID = id;
}
case class IntLit(id: ExpID, lit: Int) extends Expression(id) {
  override def toString: String = lit.toString;
}
case class FloatLit(id: ExpID, lit: Float) extends Expression(id) {
  override def toString: String = lit.toString;
}
case class UnitLit(id: ExpID) extends Expression(id) {
  override def toString: String = "()";
}
case class VarName(id: ExpID, name: String) extends Expression(id) {
  override def toString: String = name;
}
case class PrimOp(id: ExpID, op: String, args: List[Expression]) extends Expression(id) {
  override def toString: String = op + "(" + args.mkString(", ") + ")";
}
case class IfStatement(id: ExpID, cond: Expression, thenBr: Expression, elseBr: Expression) extends Expression(id) {
  override def toString: String = "if(" + cond + ") {\n" + thenBr + "\n} else {\n" + elseBr + "}\n";
}
case class LetBinding(id: ExpID, varName: String, typ: Type, rhs: Expression, next: Expression) extends Expression(id) {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class VarBinding(id: ExpID, varName: String, typ: Type, rhs: Expression, next: Expression) extends Expression(id) {
  override def toString: String = "var " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class AccessExp(id: ExpID, root: Expression, label: String) extends Expression(id) {
  override def toString: String = root.toString + "." + label;
}
case class FunctionCall(id: ExpID, function: Expression, args: List[Expression]) extends Expression(id) {
  override def toString: String = function.toString + "(" + args.mkString(", ") + ")";
}
case class ClassExpression(id: ExpID, args: List[(String, Type)], methods: Map[String, LambdaExpression]) extends Expression(id) {
  override def toString: String = {
    val argStr = args.map(elem => elem._1 + ": " + elem._2).mkString(", ");
    val bodyStr = methods.map((elem) => elem._1 + ": " + elem._2).mkString(",\n");
    "class (" + argStr + ")" + " => {\n" + bodyStr.split("\n").map(elem => " " + elem).mkString("\n") + "\n}"
  };
}
case class LambdaExpression(id: ExpID, attrs: List[String], args: List[(String, Type)], retType: Type, body: Expression) extends Expression(id) {
  override def toString: String = {
    val argStr = args.map(elem => elem._1 + ": " + elem._2).mkString(", ");
    val bodyStr = body.toString;
    if bodyStr.contains("\n") then
      "(" + argStr + ") => " + retType + " => {\n" + bodyStr.split("\n").map(elem => " " + elem).mkString("\n") + "\n}"
    else
      "(" + argStr + ") => " + retType + " => " + bodyStr
  };
}

class BlankParser {

  private var index: Int = 0;
  private var tokens: List[Token] = List();

  def parse(src: String): Expression = {
    val tokenizer = new Tokenizer(0, src);
    val tokens = tokenizer.getStream;
    this.index = 0;
    this.tokens = tokens;
    parseProgram()
  }

  private def capData(startData: TokenData): ExpID = {
    val expData = ExpressionData(
      TokenData(startData.startIndex, tokens(index).getData.endIndex),
      UnknownType()
    );
    val id = ExpID(uniqInd())
    ExpressionDataMap.put(id, expData);
    id
  }

  private def getData: ExpID = {
    val tokenData = tokens(index - 1).getData;
    val expData = ExpressionData(
      tokenData,
      UnknownType()
    );
    val id = ExpID(uniqInd())
    ExpressionDataMap.put(id, expData);
    id
  }

  private def parseProgram(): Expression = {
    val expr = parseStatement();
    tokens(index) match {
      case EOF(data) => ()
      case _ =>
        raiseError(tokens(index).getData, "Expected EOF");
    };
    expr
  }

  private def parseStatement(): Expression = {
    tokens(index) match {
      case Keyword(data, "let") => {
        index += 1;
        val name = acceptId();
        val typ = attemptParseType();
        expectOp("=");
        val rhs = parseExpr();

        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            LetBinding(combineData(data, endData), name, typ, rhs, next)
          case _ =>
            LetBinding(capData(data), name, typ, rhs, UnitLit(getData))
        }
      }
      case Keyword(data, "var") => {
        index += 1;
        val name = acceptId();
        val typ = attemptParseType();
        expectOp("=");
        val rhs = parseExpr();

        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            VarBinding(combineData(data, endData), name, typ, rhs, next)
          case _ =>
            VarBinding(capData(data), name, typ, rhs, UnitLit(getData))
        }
      }
      case Keyword(data, "virtual") => {
        expectKeyword("virtual");
        expectKeyword("fn");
        val name = acceptId();
        val rhs = parseLambda() match {
          case LambdaExpression(id, attrs, args, retType, body) =>
            LambdaExpression(id, "virtual" :: attrs, args, retType, body)
        };
        val rhsType = FunType(rhs.attrs, rhs.args.map((pair) => pair._2), rhs.retType);
        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            LetBinding(combineData(data, endData), name, rhsType, rhs, next)
          case _ =>
            LetBinding(capData(data), name, rhsType, rhs, UnitLit(getData))
        }
      }
      case Keyword(data, "fn") => {
        expectKeyword("fn");
        val name = acceptId();
        val rhs = parseLambda();
        val rhsType = FunType(rhs.attrs, rhs.args.map((pair) => pair._2), rhs.retType);
        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            LetBinding(combineData(data, endData), name, rhsType, rhs, next)
          case _ =>
            LetBinding(capData(data), name, rhsType, rhs, UnitLit(getData))
        }
      }
      case Keyword(data, "class") => {
        index += 1;
        val name = acceptId();
        val rhs = parseClass(data);
        val rhsType = FunType(List(), rhs.args.map((pair) => pair._2), TypeVar("?" + uniqInd()));
        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            LetBinding(combineData(data, endData), name, rhsType, rhs, next)
          case _ =>
            LetBinding(capData(data), name, rhsType, rhs, UnitLit(getData))
        }
      }
      case _ => {
        val data = getData;
        val expr = parseExpr();
        tokens(index) match {
          case Delim(endData, ";") =>
            expectDelim(";");
            val next = parseStatement();
            LetBinding(combineData(ExpressionDataMap.getTokenData(data), endData), "val" + uniqInd(), TypeVar("?" + uniqInd()), expr, next)
          case _ =>
            expr
        }
      }
    }
  }

  private def parseType(): Type = {
    tokens(index) match {
      case Id(data, str) => BaseType(acceptId())
      case Delim(data, "{") =>
        expectDelim("{");
        expectDelim("}");
        UnitType()
      case Delim(data, "(") =>
        expectDelim("(")
        var args: List[Type] = List()
        tokens(index) match {
          case Delim(data, ")") => expectDelim(")")
          case _ => {
            args = List(parseType())
            while(tokens(index) match {
              case Delim(data, ",") => true
              case Delim(data, ")") => false
              case _ => {
                raiseError(tokens(index).getData, "Unable to parse function type argument list.");
                false
              }
            }) {
              expectDelim(",")
              args = args ++ List(parseType())
            }
            expectDelim(")")
          }
        }
        expectOp("=");
        expectOp(">");
        val resType = parseType();
        FunType(List(), args, resType)
      case _ => raiseError(tokens(index).getData, "Unknown token encountered when parsing type."); UnitType()
    }
  }

  private def attemptParseType(): Type = {
    tokens(index) match {
      case Delim(data, ":") => {
        expectDelim(":");
        parseType()
      }
      case _ => {
        TypeVar("?" + uniqInd())
      }
    }
  }

  private def parseArg(): (String, Type) = {
    (acceptId(), attemptParseType())
  }

  private def parseLambda(): (LambdaExpression) = {
    val data = expectDelim("(");
    var args: List[(String, Type)] = List()
    tokens(index) match {
      case Delim(data, ")") => expectDelim(")");
      case Id(data, str) =>
        val (name, typ) = parseArg()
        args = args ++ List((name, typ))
        while(tokens(index) match {
          case Delim(data, ",") => true
          case Delim(data, ")") => false
          case _ => {
            raiseError(tokens(index).getData, "Expected delimiter token in args.");
            false
          }
        }) {
          index += 1;
          val (name, typ) = parseArg()
          args = args ++ List((name, typ))
        }
        expectDelim(")");
      case _ => {
        raiseError(tokens(index).getData, "Expected function argument list.");
      }
    }
    val retTyp = attemptParseType();
    expectOp("=");
    expectOp(">");
    val body = parseExpr();
    LambdaExpression(combineData(data, ExpressionDataMap.getTokenData(body)), List(), args, retTyp, body)
  }

  private def parseClass(data: TokenData): ClassExpression = {
    expectDelim("(");
    var args: List[(String, Type)] = List()
    tokens(index) match {
      case Delim(data, ")") => expectDelim(")");
      case Id(data, str) =>
        val (name, typ) = parseArg()
        args = args ++ List((name, typ))
        while(tokens(index) match {
          case Delim(data, ",") => true
          case Delim(data, ")") => false
          case _ => {
            raiseError(tokens(index).getData, "Expected delimiter token in args.");
            false
          }
        }) {
          index += 1;
          val (name, typ) = parseArg()
          args = args ++ List((name, typ))
        }
        expectDelim(")");
      case _ => {
        raiseError(tokens(index).getData, "Expected function argument list.");
      }
    }
    val body = parseExpr();
    var exp = body;
    var map: Map[String, LambdaExpression] = Map();
    while(exp match { case LetBinding(id, varName, typ, rhs, next) => true; case _ => false }) {
      exp match {
        case LetBinding(id, varName, typ, rhs, next) => {
          rhs match {
            case lambda@LambdaExpression(id, attrs, args, retType, body) => {
              map = map + (varName -> lambda);
            }
            /*case ClassExpression(id, args, methods) => { }*/
            case _ => raiseError(exp.getID, "Expected function definition for class.")
          }
          exp = next
        }
        case UnitLit(id) => ()
        case _ => raiseError(exp.getID, "Expected function definition for class.")
      }
    }
    ClassExpression(combineData(data, ExpressionDataMap.getTokenData(body)), args, map)
  }

  private val precMap = Map(
    "=" -> 0,
    ">" -> 1,
    "<" -> 1,
    "+" -> 2,
    "-" -> 2,
    "*" -> 3,
    "/" -> 3,
  );
  private val assocMap = Map(
    "=" -> 0,
    "+" -> 1,
    "-" -> 1,
    "*" -> 1,
    "/" -> 1,
    ">" -> 1,
    "<" -> 1,
  );
  // 0 -> (1 + (1 + 1))
  // 1 -> ((1 + 1) + 1)

  private def parseExpr(): Expression = {
    parseExpr(0)
  }

  private def parseExpr(prec: Int): Expression = {
    var expr = parseCluster();
    var continue = true;
    while (prec <= 1 && continue) {
      val token = tokens(index);
      token match {
        case Op(data, str) =>
          expectOp(str);
          if (precMap.contains(str) && precMap(str) >= prec) {
            val innerExpr = parseExpr(precMap(str) + assocMap(str));
            expr = PrimOp(combineData(ExpressionDataMap.getTokenData(expr), ExpressionDataMap.getTokenData(innerExpr)), str, List(expr, innerExpr));
          } else
            continue = false;
        case _ => continue = false;
      }
    }
    expr
  }

  private def parseCluster(): Expression = {
    var expr = parseAtom();
    boundary {
      while (true) {
        tokens(index) match {
          case Delim(data, "(") =>
            // Function Application
            expectDelim("(");
            var args: List[Expression] = List()
            tokens(index) match {
              case Delim(data, ")") => expectDelim(")");
              case _ =>
                val expr = parseExpr()
                args = args ++ List(expr)
                while (tokens(index) match {
                  case Delim(data, ",") => true
                  case Delim(data, ")") => false
                  case _ => {
                    raiseError(tokens(index).getData, "Unexpected delimiter token in args.");
                    false // Never will run.
                  }
                }) {
                  index += 1;
                  val expr = parseExpr()
                  args = args ++ List(expr)
                }
                expectDelim(")");
              case _ =>
                raiseError(tokens(index).getData, "Expected function application argument");
            }
            expr = FunctionCall(capData(ExpressionDataMap.getTokenData(expr)), expr, args)
          case Delim(data, ".") =>
            expectDelim(".")
            val id = acceptId();
            expr = AccessExp(capData(ExpressionDataMap.getTokenData(expr)), expr, id)
          case _ => break();
        }
      }
    }
    expr
  }

  private def parseAtom(): Expression = {
    tokens(index) match {
      case Keyword(data, "new") => {
        expectKeyword("new");
        tokens(index) match {
          case Delim(data, "{") => {
            // Anonymous structure
            UnitLit(getData)
          }
          case Id(data, str) => {
            val id = acceptId();
            UnitLit(getData)
            // TODO
          }
        }
      }
      case Keyword(data, "if") => {
        expectKeyword("if");
        expectDelim("(");
        val cond = parseExpr();
        expectDelim(")");
        val thenBr = parseExpr();

        val elseBr = tokens(index) match {
          case Keyword(data, "else") => {
            expectKeyword("else")
            parseExpr()
          }
          case _ => {
            UnitLit(getData)
          }
        }
        IfStatement(capData(data), cond, thenBr, elseBr)
      }
      case Delim(data, "{") => {
        expectDelim("{");
        tokens(index) match {
          case Delim(data, "}") =>
            expectDelim("}");
            UnitLit(getData)
          case _ =>
            val expr = parseStatement();
            expectDelim("}");
            expr
        }
      }
      case Delim(data, "(") => {
        parseLambda()
      }
      case TokenIntLit(data, value) =>
        index += 1;
        IntLit(getData, value.toInt)
      case TokenFloatLit(data, value) =>
        index += 1;
        FloatLit(getData, value.toFloat)
      case Id(data, str) =>
        index += 1;
        VarName(getData, str)
      case EOF(data) => UnitLit(getData)
      case Delim(data, _) => UnitLit(getData)
      case _ =>
        raiseError(tokens(index).getData, "Expected atom");
        UnitLit(getData)
    }
  }

  // Parser helper functions

  private def acceptId(): String = {
    tokens(index) match {
      case Id(data, str) => {
        index += 1;
        str
      }
      case _ => {
        raiseError(tokens(index).getData, "Expected identifier");
        ""
      }
    }
  }

  private def expectKeyword(str: String): TokenData = {
    tokens(index) match {
      case Keyword(data, keyword) => {
        if (keyword == str) {
          index += 1;
          data
        } else
          raiseError(tokens(index).getData, "Expected keyword '" + str + "'.");
          data
      }
      case _ => {
        raiseError(tokens(index).getData, "Expected keyword '" + str + "'.");
        TokenData(0, 0)
      }
    }
  }

  private def expectDelim(str: String): TokenData = {
    tokens(index) match {
      case Delim(data, delim) => {
        if(str == delim) {
          index += 1;
          data
        } else
          raiseError(tokens(index).getData, "Expected delimiter '" + str + "'.");
          data
      }
      case _ => {
        raiseError(tokens(index).getData, "Expected delimiter '" + str + "'.");
        TokenData(0, 0)
      }
    }
  }

  private def expectOp(str: String): TokenData = {
    tokens(index) match {
      case Op(data, op) => {
        if (str == op)
          index += 1;
        else
          raiseError(tokens(index).getData, "Expected operator '" + str + "'.");
        data
      }
      case _ => {
        raiseError(tokens(index).getData, "Expected operator '" + str + "'.");
        TokenData(0, 0)
      }
    }
  }

  private def expectInt(): TokenIntLit = {
    tokens(index) match {
      case intlit@TokenIntLit(_, _) =>
        index += 1;
        intlit
      case _ => {
        raiseError(tokens(index).getData, "Expected integer literal.");
        TokenIntLit(TokenData(0, 0), "0")
      }
    }
  }

}
