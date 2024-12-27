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
          case RhsDefF(cont, _, _) => {
            val newBindings = convertRhsToLLVM(rhs, varName, typ, bindings, cont);
            convertTopLevelLLVM(next, newBindings)
          }
          case _ => {
            convertTopLevelLLVM(next, bindings)
          }
        }
      case IREOF() => ()
    }
  }

  def convertIRToLLVM(
     ir: IRExp,
     bindings: Map[String, (IRType, String)],
     retCont: String
  ): Map[String, (IRType, String)] = {
    ir match {
      case IRLet(varName, typ, rhs, next) =>
        val newBindings = convertRhsToLLVM(rhs, varName, typ, bindings, retCont);
        val newnewBindings = convertIRToLLVM(next, newBindings, retCont);
        newnewBindings
      case IRCallC(cont, varName) => {
        if(retCont == cont) {
          addLine("  ret " + bindings(varName)._1 + " " + bindings(varName)._2);
        } else {
          addLine("  store " + bindings(varName)._1 + " " + bindings(varName)._2 + ", ptr " + bindings(cont)._2);
        }
        bindings
      }
      case IRCallF(function, cont, args) => {
        val retVal = generateName("retval");
        addLine("  %" + retVal + " = call " + bindings(cont)._1 + " " + bindings(function)._2 + "(" + args.map((elem) => "" + bindings(elem)._1 + " " + bindings(elem)._2).mkString(", ") + ")");
        addLine("  store " + bindings(cont)._1 + " %" + retVal + ", ptr " + bindings(cont)._2 + "r");
        addLine("  br label " + bindings(cont)._2);
        bindings
      }
    }
  }

  def convertRhsToLLVM(
    rhs: IRRHS,
    varName: String,
    typ: IRType,
    bindings: Map[String, (IRType, String)],
    retCont: String,
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
          case "=" =>
            addLine("  store " + typ + " " + bindings(args(1))._2 + ", ptr %" + args.head)
            val name = generateName("var" + varName);
            addLine("  %" + name + " = load " + typ + ", ptr %" + args.head);
            bindings + (varName -> (typ, "%" + name))
        }
      }
      case RhsAlloc(typ) => {
        addLine("%" + varName + " = alloca " + typ);
        bindings + (varName -> (typ, "%" + varName))
      }
      case RhsDefF(cont, args, body) => {
        addLine("define i32 @" + varName + "(" + args.map((arg) => "" + arg._2 + " %" + arg._1).mkString(", ") + ") {");
        // Make new bindings.
        var newBindings = bindings;
        for(arg <- args) {
          newBindings = newBindings + (arg._1 -> (arg._2, "%" + arg._1));
        }
        convertIRToLLVM(body, newBindings, cont);
        addLine("}");
        addLine("");
        bindings + (varName -> (IRPtr(), "@" + varName))
      }
      case RhsDefC(args, contBody) => {
        val next = generateName("next");
        addLine("  %" + varName + "r = alloca " + typ);
        addLine("  br label %" + next);
        addLine(varName + ":");
        addLine("  %" + args.head + " = load " + typ + ", ptr %" + varName + "r");
        val bodyBindings = bindings + (args.head -> (typ, "%" + args.head));
        convertIRToLLVM(contBody, bodyBindings, retCont);
        addLine(next + ":");
        bindings + (varName -> (typ, "%" + varName))
      }
    }
  }

}