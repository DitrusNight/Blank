package blank

import scala.util.matching.Regex

object BlankParser {

  abstract class Token
  case class Id(str: String) extends Token
  case class IntLit(str: String) extends Token
  case class FloatLit(str: String) extends Token
  case class Op(sym: Char) extends Token
  case class Delim(sym: Char) extends Token
  case class EOF() extends Token

  class Tokenizer(var index: Int, var src: String) {

    def consumeWhitespace() = {
      while(index < src.length && (
        if index < src.length then src.charAt(index) == ' ' || src.charAt(index) == '\t' || src.charAt(index) == '\n'
        else false
      )) {
        index += 1;
      }
    }

    def getToken(): Token = {
      val idRegex: Regex = "^([a-zA-Z_$][0-9a-zA-Z_$]*).*".r;
      val floatLitRegex: Regex = "^([0-9]+\\.[0-9]+).*".r;
      val intLitRegex: Regex = "^([1-9][0-9]*).*".r;
      val delimRegex: Regex = "^([{};,()]).*".r;
      val substr = src.substring(index);

      substr match {
        case idRegex(str) => {
          index += str.length;
          consumeWhitespace();
          Id(str)
        }
        case floatLitRegex(str) => {
          index += str.length;
          consumeWhitespace();
          FloatLit(str)
        }
        case intLitRegex(str) => {
          index += str.length;
          consumeWhitespace();
          IntLit(str)
        }
        case delimRegex(str) => {
          val token = Delim(src.charAt(index));
          index += 1;
          consumeWhitespace();
          token
        };
        case _ => {
          val token = Op(src.charAt(index));
          index += 1;
          consumeWhitespace();
          token
        };
      }
    }

    def getStream() = {
      var list: List[Token] = List();
      while(index < src.length) {
        list = getToken() :: list;
      }
      list = EOF() :: list;
      list.reverse
    }
  }

  def parse(src: String) = {
    val tokenizer = new Tokenizer(0, src);
    val tokens = tokenizer.getStream();
    println(tokens.toString());
    val (expr, _) = parseProgram(0, tokens);
    println(expr.toString);
  }

  def expectId(index: Int, tokens: List[Token]): String = {
    tokens(index) match {
      case Id(str) => str
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected id but got " + tokens(index));
      }
    }
  }

  def expectDelim(index: Int, tokens: List[Token], c: Char): Unit = {
    tokens(index) match {
      case Delim(delimChar) => {
        if(c == delimChar)
          ()
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

  def expectOp(index: Int, tokens: List[Token], c: Char): Unit = {
    tokens(index) match {
      case Op(opChar) => {
        if (c == opChar)
          ()
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

  def expectInt(index: Int, tokens: List[Token]): IntLit = {
    tokens(index) match {
      case intlit@IntLit(_) => intlit
      case _ => {
        // TODO Handle errors.
        throw new RuntimeException("Expected int literal but got " + tokens(index));
      }
    }
  }

  abstract class Expression
  case class Lit(lit: IntLit) extends Expression
  case class Add(arg1: Expression, arg2: Expression) extends Expression
  case class Times(arg1: Expression, arg2: Expression) extends Expression
  case class Minus(arg1: Expression, arg2: Expression) extends Expression
  case class Div(arg1: Expression, arg2: Expression) extends Expression

  val precMap = Map(
    '+' -> 0,
    '-' -> 0,
    '*' -> 1,
    '/' -> 1,
  );

  def parseAtom(startIndex: Int, tokens: List[Token]): (Expression, Int) = {
    tokens(startIndex) match {
      case Delim('(') =>
        val (expr, index) = parseExpr(startIndex + 1, tokens, 0);
        expectDelim(index, tokens, ')');
        (expr, index + 1)
      case intlit@IntLit(_) =>
        (Lit(intlit), startIndex + 1)
      case _ =>
        // TODO Handle errors
        throw new RuntimeException("Expected atom but got " + tokens(startIndex));
    }
  }

  def parseExpr(startIndex: Int, tokens: List[Token], prec: Int): (Expression, Int) = {
    var (expr, index) = parseAtom(startIndex, tokens);
    var continue = true;
    while (prec <= 1 && continue) {
      val token = tokens(index);
      token match {
        case Op(char) =>
          if (precMap.contains(char) && precMap(char) >= prec) {
            val (innerExpr, newIndex) = parseExpr(index + 1, tokens, prec + 1);
            expr = char match {
              case '+' => Add(expr, innerExpr)
              case '-' => Minus(expr, innerExpr)
              case '*' => Times(expr, innerExpr)
              case '/' => Div(expr, innerExpr)
            }
            index = newIndex;
          } else
            continue = false;
        case _ => continue = false;
      }
    }
    (expr, index)
  }

  def parseProgram(startIndex: Int, tokens: List[Token]): (Expression, Int) = {
    val (expr, index) = parseExpr(startIndex, tokens, 0)
    tokens(index) match {
      case EOF() => ()
      case _ =>
        // TODO Handle errors
        throw new RuntimeException("Expected EOF but got " + tokens(startIndex));
    }
    (expr, index + 1)
  }

}
