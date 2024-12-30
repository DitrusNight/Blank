package blank

import blank.ErrorHandler.{combineData, raiseError}

import scala.util.boundary
import scala.util.boundary.break

abstract class Expression(data: TokenData) {
  def getData: TokenData = data;
}

case class IntLit(data: TokenData, lit: Int) extends Expression(data) {
  override def toString: String = lit.toString;
}
case class FloatLit(data: TokenData, lit: Float) extends Expression(data) {
  override def toString: String = lit.toString;
}
case class UnitLit(data: TokenData) extends Expression(data) {
  override def toString: String = "()";
}
case class VarName(data: TokenData, name: String) extends Expression(data) {
  override def toString: String = name;
}
case class PrimOp(data: TokenData, op: String, args: List[Expression]) extends Expression(data) {
  override def toString: String = op + "(" + args.mkString(", ") + ")";
}
case class IfStatement(data: TokenData, cond: Expression, thenBr: Expression, elseBr: Expression) extends Expression(data) {
  override def toString: String = "if(" + cond + ") {\n" + thenBr + "\n} else {\n" + elseBr + "}\n";
}
case class LetBinding(data: TokenData, varName: String, typ: Type, rhs: Expression, next: Expression) extends Expression(data) {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class VarBinding(data: TokenData, varName: String, typ: Type, rhs: Expression, next: Expression) extends Expression(data) {
  override def toString: String = "var " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class AccessExp(data: TokenData, root: Expression, label: String) extends Expression(data) {
  override def toString: String = root.toString + "." + label;
}
case class FunctionCall(data: TokenData, function: Expression, args: List[Expression]) extends Expression(data) {
  override def toString: String = function.toString + "(" + args.mkString(", ") + ")";
}
case class ClassExpression(data: TokenData, args: List[(String, Type)], body: Expression) extends Expression(data) {
  override def toString: String = {
    val argStr = args.map(elem => elem._1 + ": " + elem._2).mkString(", ");
    val bodyStr = body.toString;
    "class (" + argStr + ")" + " => {\n" + bodyStr.split("\n").map(elem => " " + elem).mkString("\n") + "\n}"
  };
}
case class LambdaExpression(data: TokenData, args: List[(String, Type)], retType: Type, body: Expression) extends Expression(data) {
  override def toString: String = {
    val argStr = args.map(elem => elem._1 + ": " + elem._2).mkString(", ");
    val bodyStr = body.toString;
    if bodyStr.contains("\n") then
      "(" + argStr + "): " + retType + " => {\n" + bodyStr.split("\n").map(elem => " " + elem).mkString("\n") + "\n}"
    else
      "(" + argStr + "): " + retType + " => " + bodyStr
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

  private def capData(startData: TokenData) = {
    TokenData(startData.startIndex, tokens(index).getData.endIndex);
  }

  private def getData: TokenData = tokens(index).getData;

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
      case Keyword(data, "fn") => {
        index += 1;
        val name = acceptId();
        val rhs = parseLambda();
        val rhsType = FunType(rhs.args.map((pair) => pair._2), rhs.retType);
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
        val rhsType = FunType(rhs.args.map((pair) => pair._2), TypeVar("?" + uniqInd()));
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
            LetBinding(combineData(data, endData), "val" + uniqInd(), TypeVar("?" + uniqInd()), expr, next)
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
        FunType(args, resType)
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
    LambdaExpression(combineData(data, body.getData), args, retTyp, body)
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
    ClassExpression(combineData(data, body.getData), args, body)
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
            expr = PrimOp(combineData(expr.getData, innerExpr.getData), str, List(expr, innerExpr));
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
            expr = FunctionCall(capData(expr.getData), expr, args)
          case Delim(data, ".") =>
            expectDelim(".")
            val id = acceptId();
            expr = AccessExp(capData(expr.getData), expr, id)
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
            UnitLit(data)
          }
          case Id(data, str) => {
            val id = acceptId();
            UnitLit(data)
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
        IntLit(data, value.toInt)
      case TokenFloatLit(data, value) =>
        index += 1;
        FloatLit(data, value.toFloat)
      case Id(data, str) =>
        index += 1;
        VarName(data, str)
      case EOF(data) => UnitLit(data)
      case Delim(data, _) => UnitLit(data)
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
