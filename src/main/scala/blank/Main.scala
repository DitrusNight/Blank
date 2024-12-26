package blank

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = args(0);
    val fileHandler = Source.fromFile(file);
    val src = fileHandler.getLines().mkString("\n");
    fileHandler.close();
    val ast = new BlankParser().parse(src);
    val typeAnalyzer = new Types();
    typeAnalyzer.populateArithmeticTypes();
    println(ast.toString);
    typeAnalyzer.inferType(Map(), ast);
    val newAST = typeAnalyzer.convertTypes(Map(), ast, (exp) => exp);
    println(newAST.toString);
    val ir = IR.convertASTToIR(newAST, IR.identityCont);
    println(ir.toString);
  }

}