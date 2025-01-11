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

  def convertIRProgram(program: IRProgram): Unit = {
    initGlobalItems();
    var bindings: Map[String, BindingData] = Map();
    for(className <- IRTypes.vmtMap.keys) {
      bindings = bindings + ((className + "$_vmt") -> BindingData(IRVmt(className), "@" + className + "$_vmt"))
    }
    program.methods.foreach((methodPair) => {
      val methodName = methodPair._1;
      val method = methodPair._2;
      method.retTyp match {
        case IRClass(className) =>
        case _ => ()
      }
      bindings = bindings + (methodName -> BindingData(IRFuncPtr(method.args.map((pair) => pair._2), method.retTyp), "@" + methodName))
    })
    program.methods.foreach((methodPair) => {
      val methodName = methodPair._1;
      val method = methodPair._2;
      val newBindings = bindings + (method.cont -> BindingData(IRCont(List(method.retTyp)), "~~UNUSED~~")) ++ method.args.map(pair => (pair._1 -> BindingData(pair._2, "%" + pair._1)));
      addLine("define " + method.retTyp.outputLLVM + " @" + methodName + "(" + method.args.map((arg) => "" + arg._2.outputLLVM + " %" + arg._1).mkString(", ") + ") {");

      convertIRToLLVM(method.body, newBindings, method.cont);

      addLine("}");
      addLine("");
    })
  }

  def initGlobalItems(): Unit = {
    for(className <- IRTypes.classMap.keySet) {
      val vmtStruct = IRTypes.vmtMap(className)._2;
      val classStruct = IRTypes.classMap(className)._2;
      addLine("%" + IRTypes.vmtMap(className)._1 + " = type { " + vmtStruct.methods.values.map((elem) => elem.outputLLVM).mkString(", ") + " }");
      addLine("%" + IRTypes.classMap(className)._1 + " = type { " + IRVarPtr(IRVmt(className)).outputLLVM + ", " + classStruct.fields.values.map((elem) => elem.outputLLVM).mkString(", ") + " }");
      addLine("@" + className + "$_vmt = constant %" + IRTypes.vmtMap(className)._1 + " { " + vmtStruct.methods.map((pair) => "ptr @" + className + "$" + pair._1).mkString(", ") + " }");
      addLine("");
    }
    for(funcName <- IRTypes.funcMap.keySet) {
      val funcStruct = IRTypes.funcMap(funcName)
      addLine("%" + funcName + " = type { ptr" + funcStruct.vars.values.map((elem) => ", " + elem.outputLLVM).mkString + " }");
    }
  }

  def generateName(prefix: String = "name"): String = {
    prefix + uniqInd()
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
          case IRWrappedFunc(funcName) => if(label == "_func") 0 else {
            IRTypes.funcMap(funcName).vars.keys.toList.indexOf(label) + 1
          }
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
      case RhsFuncAlloc(func, map) => {
        val funcName = typ match { case IRWrappedFunc(funcName) => funcName};
        val structName = "%" + funcName;
        val sizePtr = "%" + generateName("sizePtr");
        val sizeInt = "%" + generateName("sizeInt");

        addLine("  " + sizePtr + " = getelementptr " + typ.outputLLVM + ", ptr null, i32 1");
        addLine("  " + sizeInt + " = ptrtoint " + typ.outputLLVM + " " + sizePtr + " to i64");

        addLine("  %" + varName + " = call ptr @malloc(i64 " + sizeInt + ")");
        bindings + (varName -> BindingData(typ, "%" + varName))
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