package blank

case class BindingData(typ: IRType, varName: String) {
  def output: String = typ.outputLLVM + " " + varName
}

object LLVM {

  var context: List[String] = List(
    "target triple = \"x86_64-pc-linux-gnu\"",
    "",
    "declare ptr @malloc(i64)", // #1
    //"attributes #0 = { nounwind ssp uwtable “less-precise-fpmad”=“false” “no-frame-pointer-elim”=“true” “no-frame-pointer-elim-non-leaf” “no-infs-fp-math”=“false” “no-nans-fp-math”=“false” “stack-protector-buffer-size”=“8” “unsafe-fp-math”=“false” “use-soft-float”=“false” }",
    //"attributes #1 = { “less-precise-fpmad”=“false” “no-frame-pointer-elim”=“true” “no-frame-pointer-elim-non-leaf” “no-infs-fp-math”=“false” “no-nans-fp-math”=“false” “stack-protector-buffer-size”=“8” “unsafe-fp-math”=“false” “use-soft-float”=“false” }"
  );

  private def addLine(str: String): Unit = {
    context = context ++ List(str);
  }

  def initClasses(): Unit = {
    for(className <- IRTypes.classMap.keySet) {
      val vmtStruct = IRTypes.vmtMap(className)._2;
      val classStruct = IRTypes.classMap(className)._2;
      addLine("%" + IRTypes.vmtMap(className)._1 + " = type { " + vmtStruct.methods.values.map((elem) => elem.outputLLVM).mkString(", ") + " }");
      addLine("%" + IRTypes.classMap(className)._1 + " = type { " + IRVarPtr(IRVmt(className)).outputLLVM + ", " + classStruct.fields.values.map((elem) => elem.outputLLVM).mkString(", ") + " }");
      addLine("@" + className + "$_vmt = constant %" + IRTypes.vmtMap(className)._1 + " { " + vmtStruct.methods.map((pair) => "ptr @" + className + "$" + pair._1).mkString(", ") + " }");
      addLine("");
    }
  }

  def getGlobalBindings(exp: IRExp): Map[String, BindingData] = {
    IRTypes.vmtMap.map((elem) => (elem._1 + "$_vmt") -> BindingData(IRVmt(elem._1), "@" + elem._1 + "$_vmt"))
    ++ getExpBindings(exp)
  }

  def getExpBindings(exp: IRExp): Map[String, BindingData] = {
    exp match {
      case IRLet(varName, typ, rhs, next) => {
        val map1 = rhs match {
          case RhsDefF(cont, attrs, args, body, retType) => Map(varName -> BindingData(IRFuncPtr(attrs, args.map((pair) => pair._2), retType), "@" + varName))
          case _ => Map()
        }
        map1 ++ getExpBindings(next)
      }
      case IRVar(varName, typ, rhs, next) => getExpBindings(next)
      case _ => Map()
    }
  }

  /*
  def convertStructTypesExp(bindings: Map[String, IRType], exp: IRExp): Unit = {
    def checkType(typ: IRType): Unit = {
      typ match {
        case struct@IRStruct(className, map) =>
          if(!structTypes.keySet.toList.contains(struct)) {
            val name = "%" + generateName("struct.");
            structTypes = structTypes + (struct -> name)
            addLine(name + " = type " + struct.outputLLVM);
          }
        case _ => ()
      }
    }
    def convertStructTypesRhs(bindings: Map[String, IRType], exp: IRRHS): Unit = {
      exp match {
        case RhsDefF(cont, attrs, args, body, retTyp) =>
          args.foreach((elem) => checkType(elem._2));
          checkType(retTyp);
          convertStructTypesExp(bindings ++ args, body);
        case RhsDefC(args, body) =>
          args.foreach((elem) => checkType(elem._2));
          convertStructTypesExp(bindings ++ args, body);
        case RhsAccess(root, label) =>
          bindings(root) match {
            case IRValPtr(typ) => checkType(typ)
          }
        case _ => ()
      }
    }
    exp match {
      case IRLet(varName, typ, rhs, next) =>
        convertStructTypesRhs(bindings, rhs);
        checkType(typ);
        convertStructTypesExp(bindings + (varName -> typ), next)
      case _ => ()
    }
  }
  */

  def generateName(prefix: String = "name"): String = {
    prefix + uniqInd()
  }

  def convertTopLevelLLVM(ir: IRExp, bindings: Map[String, BindingData]): Unit = {
    ir match {
      case IRLet(varName, typ, rhs, next) =>
        rhs match {
          case RhsDefF(cont, _, _, _, retTyp) => {
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
     bindings: Map[String, BindingData],
     retCont: String
  ): Map[String, BindingData] = {
    ir match {
      case IRLet(varName, typ, rhs, next) =>
        val newBindings = convertRhsToLLVM(rhs, varName, typ, bindings, retCont);
        val newnewBindings = convertIRToLLVM(next, newBindings, retCont);
        newnewBindings
      case IRCallC(cont, List(varName)) => {
        if(retCont == cont) {
          val typ = bindings(retCont).typ match { case IRCont(List(argType)) => argType };
          val value = accessVar(varName, bindings, typ)
          addLine("  ret " + typ.outputLLVM + " " + value);
        } else {
          if (bindings.contains(cont))
            addLine("  store " + bindings(varName).output + ", ptr " + bindings(cont)._2 + "r");
          addLine("  br label " + bindings(cont).varName);
        }
        bindings
      }
      case IRAccess(varName, typ, root, label, next) => {
        val structTyp = bindings(root).typ
        val innerTyp = typ
        addLine("  %" + varName + " = getelementptr inbounds " + innerTyp.outputLLVM + ", ptr " + accessVar(root, bindings, structTyp) + ", i32 " + (structTyp match {
          case IRClass(className) => if(label == "_vmt") 0 else {
            IRTypes.classMap(className)._2.fields.keys.toList.indexOf(label) + 1
          }
          case IRVmt(className) => IRTypes.vmtMap(className)._2.methods.keys.toList.indexOf(label)
        }));
        val newBindings = bindings + (varName -> BindingData(innerTyp, "%" + varName))
        val newnewBindings = convertIRToLLVM(next, newBindings, retCont);
        newnewBindings
      }
      case IRCallF(function, cont, args) => {
        if(retCont == cont) {
          val retVal = generateName("retval");
          val contArgTyp = bindings(cont).typ match { case IRCont(List(arg)) => arg }
          addLine("  %" + retVal + " = tail call " + contArgTyp.outputLLVM + " " + bindings(function).varName + "(" + args.map((elem) => "" + bindings(elem).output).mkString(", ") + ")");
          addLine("  ret " + contArgTyp.outputLLVM + " %" + retVal);
          bindings
        } else {
          val retVal = generateName("retval");
          val contArgTyp = bindings(cont).typ match { case IRCont(List(arg)) => arg }
          addLine("  %" + retVal + " = call " + contArgTyp.outputLLVM + " " + bindings(function).varName + "(" + args.map((elem) => "" + bindings(elem).output).mkString(", ") + ")");
          if (bindings.contains(cont))
            addLine("  store " + contArgTyp.outputLLVM + " %" + retVal + ", ptr " + bindings(cont).varName + "r");
          addLine("  br label " + bindings(cont).varName);
          bindings
        }
      }
      case IRIf(cond, contTrue, contFalse) => {
        addLine("  br " + bindings(cond).output + ", label " + bindings(contTrue)._2 + ", label " + bindings(contFalse)._2);
        bindings
      }
      case IRSet(varName, valueName, next) => {
        val typ = bindings(varName).typ;
        val innerTyp = typ match {
          case IRVarPtr(typ) => typ
        };
        val newValueName = accessVar(valueName, bindings, innerTyp);
        addLine("  store " + innerTyp.outputLLVM + " " + newValueName + ", " + bindings(varName).output)

        val newnewBindings = convertIRToLLVM(next, bindings, retCont);
        newnewBindings
        //bindings + (varName -> (typ, valueName)
      }
    }
  }

  def accessVar(name: String, bindings: Map[String, BindingData], typ: IRType): String = {
    if(bindings(name).typ == typ) {
      bindings(name).varName
    } else {
      val newName = "%" + generateName();
      (bindings(name).typ, typ) match {
        /*case (IRVarPtr(innerTyp), _) => {
          addLine("  " + newName + " = load " + innerTyp.outputLLVM + ", " + bindings(name).output);
          accessVar(name, bindings + (name -> BindingData(innerTyp, newName)), typ)
        }*/
        case (IRU8(), IRI16()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU8(), IRU16()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU8(), IRU32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU8(), IRI32()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU8(), IRU64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);
        case (IRU8(), IRI64()) => addLine("  " + newName + " = zext " + bindings(name)._1.outputLLVM + " " + bindings(name)._2 + " to " + typ.outputLLVM);

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
    bindings: Map[String, BindingData],
    retCont: String,
  ): Map[String, BindingData] = {
    rhs match {
      case RhsIntLit(value) => {
        bindings + (varName -> BindingData(typ, value.toString))
      }
      case RhsUnitLit() => {
        bindings + (varName -> BindingData(typ, "0"))
      }
      case RhsPrim(op, args) => {
        op match {
          case "+" =>
            addLine("  %" + varName + " = add " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(typ, "%" + varName))
          case "-" =>
            addLine("  %" + varName + " = sub " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(typ, "%" + varName))
          case "*" =>
            addLine("  %" + varName + " = mul " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(typ, "%" + varName))
          case "/" =>
            addLine("  %" + varName + " = div " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(typ, "%" + varName))
          case ">" =>
            val typ = bindings(args.head).typ match {
              case inner => inner
            }
            addLine("  %" + varName + " = icmp sgt " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(IRBoolean(), "%" + varName))
          case "<" =>
            val typ = bindings(args.head)._1 match {
              case inner => inner
              case typ@_ => typ
            }
            addLine("  %" + varName + " = icmp slt " + typ.outputLLVM + " " + args.map((elem) => accessVar(elem, bindings, typ)).mkString(", "));
            bindings + (varName -> BindingData(IRBoolean(), "%" + varName))
        }
      }
      case RhsClassAlloc(typ) => {
        val className = typ match { case IRClass(className) => className};
        val structName = "%" + IRTypes.classMap(className)._1;
        val sizePtr = "%" + generateName("sizePtr");
        val sizeInt = "%" + generateName("sizeInt");
        /*
        %Size = getelementptr %T* null, i32 1
        %SizeI = ptrtoint %T* %Size to i32
         */
        addLine("  " + sizePtr + " = getelementptr " + typ.outputLLVM + ", ptr null, i32 1");
        addLine("  " + sizeInt + " = ptrtoint " + typ.outputLLVM + " " + sizePtr + " to i64");
        // %4 = call i8* @malloc(i64 %3)
        addLine("  %" + varName + " = call ptr @malloc(i64 " + sizeInt + ")");
        bindings + (varName -> BindingData(typ, "%" + varName))
      }
      case RhsDefF(cont, attrs, args, body, retTyp) => {
        addLine("define " + retTyp.outputLLVM + " @" + varName + "(" + args.map((arg) => "" + arg._2.outputLLVM + " %" + arg._1).mkString(", ") + ") {");
        // Make new bindings.
        var newBindings = bindings + (cont -> BindingData(IRCont(List(retTyp)), "~INVALID. USED CONT~"));
        for(arg <- args) {
          newBindings = newBindings + (arg._1 -> BindingData(arg._2, "%" + arg._1));
        }
        convertIRToLLVM(body, newBindings, cont);
        addLine("}");
        addLine("");
        bindings + (varName -> BindingData(IRFuncPtr(attrs, args.map(elem => elem._2), retTyp), "@" + varName))
      }
      case RhsDefC(args, contBody) => {
        val next = generateName("next");
        val bodyBindings = if(args.nonEmpty) {
          val argType = typ match { case IRCont(List(argType)) => argType}
          addLine("  %" + varName + "r = alloca " + argType.outputLLVM);
          addLine("  br label %" + next);
          addLine(varName + ":");
          addLine("  %" + args.head._1 + " = load " + argType.outputLLVM + ", ptr %" + varName + "r");
          bindings + (args.head._1 -> BindingData(argType, "%" + args.head._1));
        } else {
          addLine("  br label %" + next);
          addLine(varName + ":");
          bindings
        }
        convertIRToLLVM(contBody, bodyBindings, retCont);
        addLine(next + ":");
        bindings + (varName -> BindingData(typ, "%" + varName))
      }
      case RhsDeref(ptrName) => {
        val typ = bindings(ptrName).typ;
        val innerTyp = typ match {
          case IRVarPtr(typ) => typ
          case _ => throw new RuntimeException("Uhh " + ptrName);
        };
        addLine("  %" + varName + " = load " + innerTyp.outputLLVM + ", " + bindings(ptrName).output);
        bindings + (varName -> BindingData(innerTyp, "%" + varName))
      }
    }
  }

}