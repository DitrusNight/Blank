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
    typeAnalyzer.inferType(Map(), ast);
    println(ast.toString);
    println(typeAnalyzer.constraints);
    val ir = IR.convertASTToIR(ast, IR.identityCont);
    println(ir.toString);
  }

}