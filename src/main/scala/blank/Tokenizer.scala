package blank

import scala.util.matching.Regex;

case class TokenData(startIndex: Int, endIndex: Int)

abstract class Token(data: TokenData) {
    def getData: TokenData = data;
}
case class Id(data: TokenData, str: String) extends Token(data)
case class Keyword(data: TokenData, str: String) extends Token(data)
case class TokenIntLit(data: TokenData, str: String) extends Token(data)
case class TokenFloatLit(data: TokenData, str: String) extends Token(data)
case class Op(data: TokenData, str: String) extends Token(data)
case class Delim(data: TokenData, str: String) extends Token(data)
case class EOF(data: TokenData) extends Token(data)

class Tokenizer(var index: Int, var src: String) {

    private def consumeWhitespace() = {
        while(index < src.length && (
        if index < src.length then src.charAt(index) == ' ' || src.charAt(index) == '\t' || src.charAt(index) == '\n'
        else false
      )) {
            index += 1;
        }
    }

    private val keywords = List(
        "let",
        "var",
        "fn",
        "class",
        "true",
        "false",
        "if",
        "else",
    )

    private def getToken: Token = {
        val idRegex: Regex = "^([a-zA-Z_$][0-9a-zA-Z_$]*)(?:.|\n|\r)*".r;
        val floatLitRegex: Regex = "^([0-9]+\\.[0-9]+)(?:.|\n|\r)*".r;
        val intLitRegex: Regex = "^([0-9]+)(?:.|\n|\r)*".r;
        val delimRegex: Regex = "^([{};,():.])(?:.|\n|\r)*".r;
        val opRegex: Regex = "^([\\-+*/=><])(?:.|\n|\r)*".r;
        val substr = src.substring(index);
        val oldIndex = index;
        substr match {
            case idRegex(str) => {
                index += str.length;
                consumeWhitespace();
                if(keywords.contains(str)) {
                    Keyword(TokenData(oldIndex, oldIndex + str.length), str)
                } else {
                    Id(TokenData(oldIndex, oldIndex + str.length), str)
                }
            }
            case floatLitRegex(str) => {
                index += str.length;
                consumeWhitespace();
                TokenFloatLit(TokenData(oldIndex, oldIndex + str.length), str)
            }
            case intLitRegex(str) => {
                index += str.length;
                consumeWhitespace();
                TokenIntLit(TokenData(oldIndex, oldIndex + str.length), str)
            }
            case delimRegex(str) => {
                val token = Delim(TokenData(oldIndex, oldIndex + str.length), str);
                index += str.length;
                consumeWhitespace();
                token
            };
            case opRegex(str) => {
                val token = Op(TokenData(oldIndex, oldIndex + str.length), str);
                index += str.length;
                consumeWhitespace();
                token
            }
            case _ => {
                // TODO Handle Errors
                throw new RuntimeException("Unknown character encountered: " + substr.charAt(0));
            }
        }
    }

    def getStream: List[Token] = {
        var list: List[Token] = List();
        while(index < src.length) {
            list = getToken :: list;
        }
        list = EOF(TokenData(index, index)) :: list;
        list.reverse
    }

}