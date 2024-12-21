package blank

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
case class PrimOp(op: String, arg1: Expression, arg2: Expression) extends Expression {
  override def toString: String = "(" + arg1 + " " + op + " " + arg2 + ")";
}
case class LetBinding(varName: String, rhs: Expression, next: Expression) extends Expression {
  override def toString: String = "let " + varName + " = " + rhs + ";\n" + next;
}
case class FunctionDef(args: List[(String, Type)], retType: Type, body: Expression) extends Expression {
  override def toString: String = {
    val argStr = args.map((elem) => elem._1 + ": " + elem._2).mkString(", ");
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

class BlankParser {

  var index: Int = 0;
  var tokens: List[Token] = List();

  def parse(src: String): Unit = {
    val tokenizer = new Tokenizer(0, src);
    val tokens = tokenizer.getStream();
    this.index = 0;
    this.tokens = tokens;
    val expr = parseProgram();
    println(expr.toString);
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
        val name = tokens(index + 1) match {
          case Id(str) => str
          case _ =>
            // TODO Handle errors
            throw new RuntimeException("Expected identifier for variable name.");
        };
        index += 2;
        expectOp('=');
        val rhs = parseExpr(0);

        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding(name, rhs, next)
          case _ =>
            LetBinding(name, rhs, UnitLit())
        }
      }
      case Keyword("fn") => {
        val name = tokens(index + 1) match {
          case Id(str) => str
          case _ =>
            // TODO Handle errors
            throw new RuntimeException("Expected identifier for variable name.");
        };
        index += 2;
        val rhs = parseLambda();
        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding(name, rhs, next)
          case _ =>
            LetBinding(name, rhs, UnitLit())
        }
      }
      case _ => {
        val expr = parseExpr(0);
        tokens(index) match {
          case Delim(';') =>
            expectDelim(';');
            val next = parseStatement();
            LetBinding("oop", expr, next)
          case _ =>
            expr
        }
      }
    }
  }

  private def parseType(): Type = {
    tokens(index) match {
      case Id(str) => {
        index += 1;
        BaseType(str)
      }
      case _ => {
        // TODO Errors
        throw new RuntimeException("Unable to parse type.");
      }
    }
  }

  private def parseArg(): (String, Type) = {
    tokens(index) match {
      case Id(name) => {
        tokens(index + 1) match {
          case Delim(':') =>
            index += 2;
            val typ = parseType();
            (name, typ)
          case _ =>
            index += 1;
            (name, TypeVar("unk" /* TODO */))
        }
      }
      case _ => {
        // TODO Errors
        throw new RuntimeException("Unable to parse arg.");
      }
    }
  }

  private def parseLambda(): (Expression) = {
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
    var retTyp: Type = TypeVar("unk");
    // Args parsed. Now attempt to parse either arrow or type.
    tokens(index) match {
      case Delim(':') =>
        expectDelim(':');
        val typParsed = parseType();
        retTyp = typParsed;
      case _ => ()
    }
    expectOp('=');
    expectOp('>');
    val body = parseExpr(0);
    FunctionDef(args, retTyp, body)
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
    var expr = parseAtom();
    var continue = true;
    while (prec <= 1 && continue) {
      val token = tokens(index);
      token match {
        case Op(char) =>
          index += 1;
          // TODO: Combine ops together into strings.
          if (precMap.contains("" + char) && precMap("" + char) >= prec) {
            val innerExpr = parseExpr(precMap("" + char) + assocMap("" + char));
            expr = PrimOp(char.toString, expr, innerExpr);
          } else
            continue = false;
        case _ => continue = false;
      }
    }
    expr
  }

  private def parseAtom(): Expression = {
    tokens(index) match {
      case Delim('{') => {
        expectDelim('{');
        val expr = parseStatement();
        expectDelim('}');
        expr
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
      case _ =>
        // TODO Handle errors
        throw new RuntimeException("Expected atom but got " + tokens(index));
    }
  }

  // Parser helper functions

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
