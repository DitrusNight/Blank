package blank

import java.io.{File, FileWriter}
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
    println(typeAnalyzer.inferType(Map(), ast));
    val newAST = typeAnalyzer.convertTypes(Map(), ast, (exp) => exp);
    println(newAST.toString);
    val ir = IR.convertASTToIR(IR.generateName(), newAST, IR.identityCont);
    println(ir.toString);
    LLVM.convertTopLevelLLVM(ir, Map());
    println(LLVM.context.mkString("\n"));
    val outFile = new File("./out/out.ll");
    val writer = new FileWriter(outFile);
    writer.write(LLVM.context.mkString("\n"));
    writer.close();
  }

}