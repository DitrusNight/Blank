package blank

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val file = args(0);
    val fileHandler = Source.fromFile(file);
    val src = fileHandler.getLines().mkString("\n");
    fileHandler.close();
    val ast = new BlankParser().parse(src);
    println(ast.toString);
    val ir = IR.convertASTToIR(ast, IR.identityCont);
    println(ir.toString);
  }

}