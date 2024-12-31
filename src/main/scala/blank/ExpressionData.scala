package blank

class ExpressionData(data: TokenData, typ: Type) {
  var tokenData: TokenData = data;
  var typeData: Type = typ;
}

object ExpressionDataMap {
  var map: Map[ExpID, ExpressionData] = Map();

  def put(id: ExpID, expressionData: ExpressionData): Unit = {
    map = map + (id -> expressionData)
  }

  def putType(id: ExpID, typ: Type): Unit = {
    val prev = map(id);
    put(id, ExpressionData(
      prev.tokenData,
      typ
    ));
  }

  def putType(exp: Expression, typ: Type): Unit = {
    val prev = map(exp.getID);
    put(exp.getID, ExpressionData(
      prev.tokenData,
      typ
    ));
  }

  def get(id: ExpID): ExpressionData = {
    map(id)
  }

  def getTokenData(id: ExpID): TokenData = {
    map(id).tokenData
  }

  def getTokenData(exp: Expression): TokenData = {
    map(exp.getID).tokenData
  }

  def getType(id: ExpID): Type = {
    map(id).typeData
  }

  def getType(exp: Expression): Type = {
    map(exp.getID).typeData
  }

}

class ExpID(id: Int) {
  def getID: Int = id;
}