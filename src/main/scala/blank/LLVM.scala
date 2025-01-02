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
          addLine("  ret " + typ.outputLLVM + " " + value);
        } else {
          if (bindings.contains(cont))
            addLine("  store " + bindings(varName)._1.outputLLVM + " " + bindings(varName)._2 + ", ptr " + bindings(cont)._2 + "r");
          addLine("  br label " + bindings(cont)._2);
        }
        bindings
      }
      case IRCallF(function, cont, args) => {
        val retVal = generateName("retval");
        addLine("  %" + retVal + " = call " + bindings(cont)._1.outputLLVM + " " + bindings(function)._2 + "(" + args.map((elem) => "" + bindings(elem)._1.outputLLVM + " " + bindings(elem)._2).mkString(", ") + ")");
        if(bindings.contains(cont))
          addLine("  store " + bindings(cont)._1.outputLLVM + " %" + retVal + ", ptr " + bindings(cont)._2 + "r");
        addLine("  br label " + bindings(cont)._2);
        bindings
      }
      case IRIf(cond, contTrue, contFalse) => {
        addLine("  br " + bindings(cond)._1.outputLLVM + " " + bindings(cond)._2 + ", label " + bindings(contTrue)._2 + ", label " + bindings(contFalse)._2);
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
          addLine("  " + newName + " = load " + innerTyp.outputLLVM + ", " + bindings(name)._1.outputLLVM + " " + bindings(name)._2);
          accessVar(name, bindings + (name -> (innerTyp, newName)), typ)
        }
        case (IRU8(), IRU16()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI16()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRU32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);
        case (IRU8(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ);

        case (IRU16(), IRU32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU16(), IRI32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU16(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU16(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);

        case (IRU32(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU32(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);

        case (IRI8(), IRI16()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRI8(), IRI32()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRI8(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);

        case (IRI16(), IRI32()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRI16(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);

        case (IRI32(), IRI64()) => addLine("  " + newName + " = sext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
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
            addLine("  %" + varName + " = add " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "-" =>
            addLine("  %" + varName + " = sub " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "*" =>
            addLine("  %" + varName + " = mul " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case "/" =>
            addLine("  %" + varName + " = div " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (typ, "%" + varName))
          case ">" =>
            val typ = bindings(args.head)._1 match {
              case IRValPtr(inner) => inner
              case typ@_ => typ
            }
            addLine("  %" + varName + " = icmp sgt " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (IRBoolean(), "%" + varName))
          case "<" =>
            val typ = bindings(args.head)._1 match {
              case IRValPtr(inner) => inner
              case typ@_ => typ
            }
            addLine("  %" + varName + " = icmp slt " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> (IRBoolean(), "%" + varName))
          case "=" =>
            val valueName = accessVar(args(1), bindings, typ);
            addLine("  store " + typ.outputLLVM + " " + valueName + ", " + bindings(args.head)._1 + " " + bindings(args.head)._2)
            bindings + (varName -> (bindings(args(1))._1, bindings(args(1))._2))
        }
      }
      case RhsAlloc(typ) => {
        addLine("  %" + varName + " = alloca " + typ.outputLLVM);
        bindings + (varName -> (IRValPtr(typ), "%" + varName))
      }
      case RhsAccess(root, label) => {
        val structTyp = bindings(root)._1
        val innerTyp = structTyp match {
          case IRStruct(map) => map(label)
        }
        addLine("  %" + varName + " = getelementptr inbounds " + structTyp.outputLLVM + ", ptr " + accessVar(root, bindings, structTyp) + ", i32 " + (structTyp match {
          case IRStruct(map) => map.toIndexedSeq.indexOf((label, innerTyp))
        }));
        bindings + (varName -> (IRValPtr(innerTyp), "%" + varName))
      }
      case RhsDefF(cont, args, body, retTyp) => {
        addLine("define " + retTyp.outputLLVM + " @" + varName + "(" + args.map((arg) => "" + arg._2.outputLLVM + " %" + arg._1).mkString(", ") + ") {");
        // Make new bindings.
        var newBindings = bindings + (cont -> (IRCont(retTyp), "~INVALID. USED CONT~"));
        for(arg <- args) {
          newBindings = newBindings + (arg._1 -> (arg._2, "%" + arg._1));
        }
        convertIRToLLVM(body, newBindings, cont);
        addLine("}");
        addLine("");
        bindings + (varName -> (IRFuncPtr(args.map(elem => elem._2), retTyp), "@" + varName))
      }
      case RhsDefC(args, contBody) => {
        val next = generateName("next");
        val bodyBindings = if(args.nonEmpty) {
          val argType = typ match { case IRCont(argType) => argType}
          addLine("  %" + varName + "r = alloca " + argType.outputLLVM);
          addLine("  br label %" + next);
          addLine(varName + ":");
          addLine("  %" + args.head + " = load " + argType.outputLLVM + ", ptr %" + varName + "r");
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