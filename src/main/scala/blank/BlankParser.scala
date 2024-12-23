package blank

import scala.util.boundary
import scala.util.boundary.break

abstract class Expression
case class Lit(lit: String) extends Expression {
  override def toString: String = lit;
}
case class UnitLit() extends Expression {
  override def toString: String = "()";
}
case class VarName(name: String) extends Expression {
  override def toString: String = name;
}
case class PrimOp(op: String, args: List[Expression]) extends Expression {
  override def toString: String = op + "(" + args.mkString(", ") + ")";
}
case class LetBinding(varName: String, typ: Type, rhs: Expression, next: Expression) extends Expression {
  override def toString: String = "let " + varName + ": " + typ + " = " + rhs + ";\n" + next;
}
case class AccessExp(root: Expression, label: String) extends Expression {
  override def toString: String = root.toString + "." + label;
}
case class FunctionCall(function: Expression, args: List[Expression]) extends Expression {
  override def toString: String = function.toString + "(" + args.mkString(", ") + ")";
}
case class LambdaExpression(args: List[(String, Type)], retType: Type, body: Expression) extends Expression {
  override def toString: String = {
    val argStr = args.map(elem => elem._1 + ": " + elem._2).mkString(", ");
    val bodyStr = body.toString;
    if bodyStr.contains("\n") then
      "(" + argStr + "): " + retType + " => {\n" + bodyStr.split("\n").map(elem => " " + elem).mkString("\n") + "\n}"
    else
      "(" + argStr + "): " + retType + " => " + bodyStr
  };
}

abstract class Type
case class BaseType(name: String) extends Type {
  override def toString: String = name;
}
case class TypeVar(name: String) extends Type {
  override def toString: String = name;
}
case class FunType(args: List[Type], ret: Type) extends Type {
  override def toString: String = "(" + args.mkString(", ") + ") => " + ret;
}

class BlankParser {

  private var index: Int = 0;
  private var tokens: List[Token] = List();
  private var uniqueIndex = 0;
  private def uniqInd: Int = {
    uniqueIndex += 1;
    uniqueIndex
  }

  def parse(src: String) = {
    val tokenizer = new Tokenizer(0, src);
    val tokens = tokenizer.getStream();
    this.index = 0;
    this.tokens = tokens;
    parseProgram()
  }

  private def parseProgram(): Expression = {
    val expr = parseStatement();
    tokens(index) match {
      case EOF() => ()
      case _ =>
        // TODO Handle errors
        throw new RuntimeException("Expected EOF but got " + tokens(index));
    };
    expr
  }

  private def parseStatement(): Expression = {
    tokens(index) match {
      case Keyword("let") => {
        index += 1;
        val name = acceptId();
        val typ = attemptParseType();
        expectOp('=');
        val rhs = parseExpr(0);

        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding(name, typ, rhs, next)
          case _ =>
            LetBinding(name, typ, rhs, UnitLit())
        }
      }
      case Keyword("fn") => {
        index += 1;
        val name = acceptId();
        val rhs = parseLambda();
        val rhsType = FunType(rhs.args.map((pair) => pair._2), rhs.retType);
        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding(name, rhsType, rhs, next)
          case _ =>
            LetBinding(name, rhsType, rhs, UnitLit())
        }
      }
      case _ => {
        val expr = parseExpr(0);
        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding("var" + uniqInd, TypeVar("?" + uniqInd), expr, next)
          case _ =>
            expr
        }
      }
    }
  }

  private def parseType(): Type = {
    tokens(index) match {
      case Id(str) => BaseType(acceptId())
      case Delim('(') =>
        expectDelim('(')
        var args: List[Type] = List()
        tokens(index) match {
          case Delim(')') => expectDelim(')')
          case _ => {
            args = List(parseType())
            while(tokens(index) match {
              case Delim(',') => true
              case Delim(')') => false
              case _ => throw new RuntimeException("Unable to parse function argument list. Unknown delimiter " + tokens(index));
            }) {
              expectDelim(',')
              args = args ++ List(parseType())
            }
            expectDelim(')')
          }
        }
        expectDelim(':');
        val resType = parseType();
        FunType(args, resType)
      case _ => throw new RuntimeException("Unknown token encountered when parsing type " + tokens(index))
    }
  }

  private def attemptParseType(): Type = {
    tokens(index) match {
      case Delim(':') => {
        expectDelim(':');
        parseType()
      }
      case _ => {
        TypeVar("?" + uniqInd)
      }
    }
  }

  private def parseArg(): (String, Type) = {
    (acceptId(), attemptParseType())
  }

  private def parseLambda(): (LambdaExpression) = {
    expectDelim('(');
    var args: List[(String, Type)] = List()
    tokens(index) match {
      case Delim(')') => expectDelim(')');
      case Id(str) =>
        val (name, typ) = parseArg()
        args = args ++ List((name, typ))
        while(tokens(index) match {
          case Delim(',') => true
          case Delim(')') => false
          case _ => throw new RuntimeException("Unexpected delimiter token in args. " + tokens(index))
        }) {
          index += 1;
          val (name, typ) = parseArg()
          args = args ++ List((name, typ))
        }
        expectDelim(')');
      case _ => {
        throw new RuntimeException("Expected function argument list but received " + tokens(index))
      }
    }
    val retTyp = attemptParseType();
    expectOp('=');
    expectOp('>');
    val body = parseExpr(0);
    LambdaExpression(args, retTyp, body)
  }

  private val precMap = Map(
    "=" -> 0,
    "+" -> 1,
    "-" -> 1,
    "*" -> 2,
    "/" -> 2,
  );
  private val assocMap = Map(
    "=" -> 0,
    "+" -> 1,
    "-" -> 1,
    "*" -> 1,
    "/" -> 1,
  );
  // 0 -> (1 + (1 + 1))
  // 1 -> ((1 + 1) + 1)

  private def parseExpr(prec: Int): Expression = {
    var expr = parseCluster();
    var continue = true;
    while (prec <= 1 && continue) {
      val token = tokens(index);
      token match {
        case Op(char) =>
          index += 1;
          // TODO: Combine ops together into strings.
          if (precMap.contains("" + char) && precMap("" + char) >= prec) {
            val innerExpr = parseExpr(precMap("" + char) + assocMap("" + char));
            expr = PrimOp(char.toString, List(expr, innerExpr));
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
          case Delim('(') =>
            // Function Application
            expectDelim('(');
            var args: List[Expression] = List()
            tokens(index) match {
              case Delim(')') => expectDelim(')');
              case _ =>
                val expr = parseExpr(0)
                args = args ++ List(expr)
                while (tokens(index) match {
                  case Delim(',') => true
                  case Delim(')') => false
                  case _ => throw new RuntimeException("Unexpected delimiter token in args. " + tokens(index))
                }) {
                  index += 1;
                  val expr = parseExpr(0)
                  args = args ++ List(expr)
                }
                expectDelim(')');
              case _ =>
                throw new RuntimeException("Expected function application argument but received " + tokens(index))
            }
            expr = FunctionCall(expr, args)
          case Delim('.') =>
            expectDelim('.')
            expr = AccessExp(expr, acceptId())
          case _ => break();
        }
      }
    }
    expr
  }

  private def parseAtom(): Expression = {
    tokens(index) match {
      case Delim('{') => {
        expectDelim('{');
        tokens(index) match {
          case Delim('}') =>
            expectDelim('}');
            UnitLit()
          case _ =>
            val expr = parseStatement();
            expectDelim('}');
            expr
        }
      }
      case Delim('(') => {
        parseLambda()
      }
      case IntLit(value) =>
        index += 1;
        Lit(value)
      case Id(str) =>
        index += 1;
        VarName(str)
      case EOF() => UnitLit()
      case Delim(_) => UnitLit()
      case _ =>
        // TODO Handle errors
        throw new RuntimeException("Expected atom but got " + tokens(index));
    }
  }

  // Parser helper functions

  private def acceptId(): String = {
    tokens(index) match {
      case Id(str) => {
        index += 1;
        str
      }
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected identifier but got " + tokens(index));
      }
    }
  }

  private def expectDelim(c: Char): Unit = {
    tokens(index) match {
      case Delim(delimChar) => {
        if(c == delimChar)
          index += 1;
        else
          // TODO Handle errors.
          throw new RuntimeException("Expected '" + c + "' but got " + tokens(index));
      }
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected delimiter '" + c + "' but got " + tokens(index));
      }
    }
  }

  private def expectOp(c: Char): Unit = {
    tokens(index) match {
      case Op(opChar) => {
        if (c == opChar)
          index += 1;
        else
          // TODO Handle errors.
          throw new RuntimeException("Expected '" + c + "' but got " + tokens(index));
      }
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected operator '" + c + "' but got " + tokens(index));
      }
    }
  }

  private def expectInt(): IntLit = {
    tokens(index) match {
      case intlit@IntLit(_) =>
        index += 1;
        intlit
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected int literal but got " + tokens(index));
      }
    }
  }

}
