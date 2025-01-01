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
          case RhsDefF(cont, _, _, retTyp) => {
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
          val typ = bindings(retCont)._1 match { case IRCont(argType) => argType };
          val value = accessVar(varName, bindings, typ)
          addLine("  ret " + typ + " " + value);
        } else {
          if (bindings.contains(cont))
            addLine("  store " + bindings(varName)._1 + " " + bindings(varName)._2 + ", ptr " + bindings(cont)._2 + "r");
          addLine("  br label " + bindings(cont)._2);
        }
        bindings
      }
      case IRCallF(function, cont, args) => {
        val retVal = generateName("retval");
        addLine("  %" + retVal + " = call " + bindings(cont)._1 + " " + bindings(function)._2 + "(" + args.map((elem) => "" + bindings(elem)._1 + " " + bindings(elem)._2).mkString(", ") + ")");
        if(bindings.contains(cont))
          addLine("  store " + bindings(cont)._1 + " %" + retVal + ", ptr " + bindings(cont)._2 + "r");
        addLine("  br label " + bindings(cont)._2);
        bindings
      }
      case IRIf(cond, contTrue, contFalse) => {
        addLine("  br " + bindings(cond)._1 + " " + bindings(cond)._2 + ", label " + bindings(contTrue)._2 + ", label " + bindings(contFalse)._2);
        bindings
      }
    }
  }

  def accessVar(name: String, bindings: Map[String, (IRType, String)], typ: IRType): String = {
    if(bindings(name)._1 == typ) {
      bindings(name)._2
    } else {
      val newName = "%" + generateName();
      (bindings(name)._1, typ) match {
        case (IRValPtr(innerTyp), _) => {
          addLine("  " + newName + " = load " + innerTyp + ", " + bindings(name)._1 + " " + bindings(name)._2);
          accessVar(name, bindings + (name -> (innerTyp, newName)), typ)
        }
        case (IRU8(), IRU16()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI16()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRU32()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI32()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);

        case (IRU16(), IRU32()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU16(), IRI32()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU16(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU16(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);

        case (IRU32(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRU32(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);

        case (IRI8(), IRI16()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRI8(), IRI32()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRI8(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);

        case (IRI16(), IRI32()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
        case (IRI16(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);

        case (IRI32(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1 + " " + bindings(name)._2 + " to " + typ);
      }
      newName
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
            addLine("  %" + varName + " = add " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "-" =>
            addLine("  %" + varName + " = sub " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "*" =>
            addLine("  %" + varName + " = mul " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "/" =>
            addLine("  %" + varName + " = div " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case ">" =>
            val typ = bindings(args.head)._1 match {
              case IRValPtr(inner) => inner
              case typ@_ => typ
            }
            addLine("  %" + varName + " = icmp sgt " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (IRBoolean(), "%" + varName))
          case "<" =>
            val typ = bindings(args.head)._1 match {
              case IRValPtr(inner) => inner
              case typ@_ => typ
            }
            addLine("  %" + varName + " = icmp slt " + typ + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (IRBoolean(), "%" + varName))
          case "=" =>
            val valueName = accessVar(args(1), bindings, typ);
            addLine("  store " + typ + " " + valueName + ", " + bindings(args.head)._1 + " " + bindings(args.head)._2)
            bindings + (varName -> (bindings(args(1))._1, bindings(args(1))._2))
        }
      }
      case RhsAlloc(typ) => {
        addLine("  %" + varName + " = alloca " + typ);
        bindings + (varName -> (IRValPtr(typ), "%" + varName))
      }
      case RhsDefF(cont, args, body, retTyp) => {
        addLine("define " + retTyp + " @" + varName + "(" + args.map((arg) => "" + arg._2 + " %" + arg._1).mkString(", ") + ") {");
        // Make new bindings.
        var newBindings = bindings + (cont -> (IRCont(retTyp), "~INVALID. USED CONT~"));
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
        val bodyBindings = if(args.nonEmpty) {
          val argType = typ match { case IRCont(argType) => argType}
          addLine("  %" + varName + "r = alloca " + argType);
          addLine("  br label %" + next);
          addLine(varName + ":");
          addLine("  %" + args.head + " = load " + argType + ", ptr %" + varName + "r");
          bindings + (args.head -> (argType, "%" + args.head));
        } else {
          addLine("  br label %" + next);
          addLine(varName + ":");
          bindings
        }
        convertIRToLLVM(contBody, bodyBindings, retCont);
        addLine(next + ":");
        val argType = typ match {case IRCont(argType) => argType}
        bindings + (varName -> (argType, "%" + varName))
      }
    }
  }

}