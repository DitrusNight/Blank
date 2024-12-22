package blank

import scala.util.matching.Regex;

abstract class Token
case class Id(str: String) extends Token
case class Keyword(str: String) extends Token
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

    val keywords = List(
        "let",
        "fn"
    )

    def getToken(): Token = {
        val idRegex: Regex = "^([a-zA-Z_$][0-9a-zA-Z_$]*)(?:.|\n)*".r;
        val floatLitRegex: Regex = "^([0-9]+\\.[0-9]+)(?:.|\n)*".r;
        val intLitRegex: Regex = "^([0-9]+)(?:.|\n)*".r;
        val delimRegex: Regex = "^([{};,():.])(?:.|\n)*".r;
        val opRegex: Regex = "^([\\-+*/=>])(?:.|\n)*".r;
        val substr = src.substring(index);

        substr match {
            case idRegex(str) => {
                index += str.length;
                consumeWhitespace();
                if(keywords.contains(str)) {
                    Keyword(str)
                } else {
                    Id(str)
                }
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
            case opRegex(str) => {
                val token = Op(src.charAt(index));
                index += 1;
                consumeWhitespace();
                token
            }
            case _ => {
                // TODO Handle Errors
                throw new RuntimeException("Unknown character encountered: " + substr.charAt(0));
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