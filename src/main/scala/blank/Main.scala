package blank

import java.io.{File, FileWriter}
import scala.io.Source
import scala.sys.process._

object Main {

  def main(args: Array[String]): Unit = {
    val file = args(0);
    val fileHandler = Source.fromFile(file);
    val src = fileHandler.getLines().mkString("\n");
    fileHandler.close();

    ErrorHandler.setSrc(src);

    val ast = new BlankParser().parse(src);
    val typeAnalyzer = new Types();
    typeAnalyzer.populateArithmeticTypes();

    println(ast.toString);
    typeAnalyzer.inferType(Map(), ast);

    val newAST = typeAnalyzer.convertTypes(Map(), ast, (exp) => exp);
    println(newAST.toString);

    val ir = IR.convertASTToIR(IR.generateName(), newAST, (varName) => IREOF());
    println(ir.toString);

    val optIr = IROpt1.performOptimizations(ir);
    println(optIr.toString);

    LLVM.convertStructTypesExp(Map(), optIr);
    LLVM.context = LLVM.context ++ List("");
    LLVM.convertTopLevelLLVM(optIr, Map());
    println(LLVM.context.mkString("\n"));

    val outFile = new File("./out/out.ll");
    val writer = new FileWriter(outFile);
    writer.write(LLVM.context.mkString("\n"));
    writer.close();
    "clang -o out/out out/out.ll".!!;
  }

}