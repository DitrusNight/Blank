package blank

object ClosureCreation {

  def createClosures(ir: IRRHS) = {
    ir match {
      case RhsDefF(cont, args, body, retTyp) => {
        ()
      }
      case _ => ()
    }
  }

  def createClosures(ir: IRExp) = {

  }



}
