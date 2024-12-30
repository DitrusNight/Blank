package blank

object ErrorHandler {

  private var sourceFileString: String = "";

  def setSrc(src: String): Unit = {
    this.sourceFileString = src;
  }

  def combineData(startData: TokenData, endData: TokenData): TokenData = {
    TokenData(startData.startIndex, endData.endIndex);
  }

  def raiseError(data: TokenData, err: String): Unit = {
    val lines = sourceFileString.split("\n");
    var startLineIndex = 0;
    var startLineOffset = 0;
    var endLineIndex = 0;
    var endLineOffset = 0;
    { // Calculate line index and offset.
      var currInd = 0;
      while (startLineIndex < lines.length && currInd < data.startIndex) {
        val offset = Math.min(data.startIndex - currInd, lines(startLineIndex).length - startLineOffset);
        currInd += offset;
        startLineOffset += offset;
        if (startLineOffset == lines(startLineIndex).length) {
          currInd += 1; // Consume new line.
          startLineIndex += 1;
          startLineOffset = 0;
        }
      }
      // Start values are calculated. Now calculate end values.
      endLineIndex = startLineIndex;
      endLineOffset = startLineOffset;
      while (endLineIndex < lines.length && currInd < data.endIndex) {
        val offset = Math.min(data.endIndex - currInd, lines(endLineIndex).length - endLineOffset);
        currInd += offset;
        endLineOffset += offset;
        if (endLineOffset == lines(endLineIndex).length && currInd != data.endIndex) {
          currInd += 1; // Consume new line.
          endLineIndex += 1;
          endLineOffset = 0;
        }
      }
    }
    // Start and end indexes are calculated.
    if (startLineIndex == endLineIndex) {
      println("Error on line " + (startLineIndex + 1));
      if (startLineIndex != 0)
        println(" - " + lines(startLineIndex - 1))
      println(" - " + lines(startLineIndex))
      println("   " + (" " * startLineOffset) + ("^" * (endLineOffset - startLineOffset)));
      println("Message: " + err);
    } else {
      println("Error on line " + (startLineIndex + 1) + " until line " + (endLineIndex + 1));
      println(" - " + lines(startLineIndex))
      println("   " + (" " * startLineOffset) + ("^" * (lines(startLineIndex).length - startLineOffset)));
      var i = startLineIndex + 1;
      while (i < Math.min(startLineIndex + 2, endLineIndex)) {
        println(" - " + lines(i))
        if (i == endLineIndex) {
          println("   " + "^" * endLineOffset);
        } else {
          println("   " + "^" * lines(i).length);
        }
        i += 1;
      }
      if (endLineIndex > startLineIndex + 2) {
        println("...");
      }
      println("Message: " + err);
    }
    System.exit(0);
  }
}
