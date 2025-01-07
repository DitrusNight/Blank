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

    println("Parsing");
    val ast = new BlankParser().parse(src);
    val typeAnalyzer = new Types();
    typeAnalyzer.populateArithmeticTypes();

    //println(ast.toString);
    println("Inferring Types");
    typeAnalyzer.inferType(Map(), ast);
    println("Converting Types");

    val newAST = typeAnalyzer.convertTypes(Map(), ast, (exp) => exp);
    println(newAST.toString);

    println("Converting to IR");
    val ir = IR.convertASTToIR(newAST, IR.getGlobalBindings(newAST), Map(), (varName, bindings) => IREOF());
    println(ir.toString);

    println("Optimizing IR");
    val optIr = IROpt1.performOptimizations(ir);
    println(optIr.toString);

    println("Creating LLVM IR");
    LLVM.context = LLVM.context ++ List("");
    LLVM.initClasses();
    LLVM.convertTopLevelLLVM(optIr, IRTypes.vmtMap.map((elem) => (elem._1 + "$_vmt") -> BindingData(IRVmt(elem._1), "@" + elem._1 + "$_vmt")));
    println(LLVM.context.mkString("\n"));

    println("Outputting LLVM IR");
    val outFile = new File("./out/out.ll");
    val writer = new FileWriter(outFile);
    writer.write(LLVM.context.mkString("\n"));
    writer.close();
    println("Compiling LLVM IR");
    "clang -g -O0 -o out/out out/out.ll".!!;
    println("Successfully compiled LLVM IR");
  }

}