package blank

object LLVM {

  var context: List[String] = List();

  def addLine(str: String): Unit = {
    context = context ++ List(str);
  }

  def generateName(prefix: String = "name"): String = {
    prefix + uniqInd()
  }

  def convertTopLevelLLVM(ir: IRExp, bindings: Map[String, (IRType, String)]): Unit = {
    ir match {
      case IRLet(varName, typ, rhs, next) =>
        rhs match {
          case RhsDefF(_, _, _) => {
            val newBindings = convertRhsToLLVM(rhs, varName, typ, bindings);
            convertTopLevelLLVM(next, newBindings)
          }
          case _ => {
            convertTopLevelLLVM(next, bindings)
          }
        }
      case IRValue(varName) => ()
    }
  }

  def convertIRToLLVM(ir: IRExp, bindings: Map[String, (IRType, String)]): Map[String, (IRType, String)] = {
    ir match {
      case IRLet(varName, typ, rhs, next) =>
        val newBindings = convertRhsToLLVM(rhs, varName, typ, bindings);
        val newnewBindings = convertIRToLLVM(next, newBindings);
        newnewBindings
      case IRValue(varName) => {
        // TODO: Store inside variable to be returned later.
        addLine("  ret i32 %" + varName);
        bindings
      }
    }
  }

  def convertRhsToLLVM(
    rhs: IRRHS,
    varName: String,
    typ: IRType,
    bindings: Map[String, (IRType, String)]
  ): Map[String, (IRType, String)] = {
    rhs match {
      case RhsIntLit(value) => {
        bindings + (varName -> (typ, value.toString))
      }
      case RhsUnitLit() => {
        bindings + (varName -> (typ, "0"))
      }
      case RhsPrim(op, args) => {
        op match {
          case "id" =>
            bindings + (varName -> (bindings(args.head)._1, bindings(args.head)._2))
          case "+" =>
            addLine("  %" + varName + " = add " + typ + " " + args.map((elem) => bindings(elem)._2).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "-" =>
            addLine("  %" + varName + " = sub " + typ + " " + args.map((elem) => bindings(elem)._2).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "*" =>
            addLine("  %" + varName + " = mul " + typ + " " + args.map((elem) => bindings(elem)._2).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "/" =>
            addLine("  %" + varName + " = div " + typ + " " + args.map((elem) => bindings(elem)._2).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
        }
      }
      case RhsAlloc(typ) => {
        addLine("%" + varName + " = alloca " + typ);
        bindings + (varName -> (typ, "%" + varName))
      }
      case RhsDefF(cont, args, body) => {
        addLine("define i32 @" + varName + "(" + args.mkString(", ") + ") {");
        // Make new bindings.
        convertIRToLLVM(body, bindings);
        addLine("}");
        bindings
      }
    }
  }

}