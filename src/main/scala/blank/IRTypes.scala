package blank

abstract class IRType {
  def outputLLVM: String;
}
case class IRU8() extends IRType {
  override def toString: String = "u8";
  override def outputLLVM: String = "i8";
}
case class IRU16() extends IRType {
  override def toString: String = "u16";
  override def outputLLVM: String = "i16";
}
case class IRU32() extends IRType {
  override def toString: String = "u32";
  override def outputLLVM: String = "i32";
}
case class IRU64() extends IRType {
  override def toString: String = "u64";
  override def outputLLVM: String = "i64";
}
case class IRI8() extends IRType {
  override def toString: String = "i8";
  override def outputLLVM: String = "i8";
}
case class IRI16() extends IRType {
  override def toString: String = "i16";
  override def outputLLVM: String = "i16";
}
case class IRI32() extends IRType {
  override def toString: String = "i32";
  override def outputLLVM: String = "i32";
}
case class IRI64() extends IRType {
  override def toString: String = "i64";
  override def outputLLVM: String = "i64";
}
case class IRF32() extends IRType {
  override def toString: String = "f32";
  override def outputLLVM: String = "f32";
}
case class IRF64() extends IRType {
  override def toString: String = "f64";
  override def outputLLVM: String = "f64";
}
case class IRVarPtr(typ: IRType) extends IRType {
  override def toString: String = "*" + typ;
  override def outputLLVM: String = typ match {
    case IRFuncPtr(args, ret) => "ptr"
    case _ => typ.outputLLVM + "*"
  };
}
case class IRFuncPtr(args: List[IRType], ret: IRType) extends IRType {
  override def toString: String = /*attrs.map(attr => "@" + attr + " ").mkString +*/ "f(" + args.mkString(", ") + ") => " + ret;
  override def outputLLVM: String = "ptr";
}
case class IRBoolean() extends IRType {
  override def toString: String = "bool";
  override def outputLLVM: String = "i1";
}
case class IRUnit() extends IRType {
  override def toString: String = "unit";
  override def outputLLVM: String = "i1";
}
/*case class IRStruct(className: String, map: Map[String, IRType]) extends IRType {
  override def toString: String = "{ " + map.keySet.toList.map((elem) => elem + ": " + map(elem).toString).mkString(",") + " }";
  override def outputLLVM: String = "{ " + map.keySet.toList.map((elem) => map(elem).outputLLVM).mkString(", ") + " }";
}*/
/*case class IRUnk() extends IRType {
  override def toString: String = "unk";
  override def outputLLVM: String = "unk";
}*/
case class IRCont(args: List[IRType]) extends IRType {
  override def toString: String = "c(" + args.mkString(", ") + ")";
  override def outputLLVM: String = "label";
}
case class IRClass(className: String) extends IRType {
  override def toString: String = "class<" + className + ">";
  override def outputLLVM: String = "%" + IRTypes.classMap(className)._1 + "*";
}
case class IRVmt(vmtName: String) extends IRType {
  override def toString: String = "vmt<" + vmtName + ">";
  override def outputLLVM: String = "%" + IRTypes.vmtMap(vmtName)._1 + "*";
}


case class ClassStruct(fields: Map[String, IRType], methods: Map[String, IRFuncPtr], vmt: IRVmt) {
  override def toString: String = "";
}
case class VmtStruct(methods: Map[String, IRFuncPtr]) {
  override def toString: String = "";
}

object IRTypes {

  var classMap: Map[String, (String, ClassStruct)] = Map();
  var vmtMap: Map[String, (String, VmtStruct)] = Map();


}
