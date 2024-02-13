package wacc

/**
  * Constants that are used throughout the program
  */
object globals {
    val exitSuccess     = 0;
    val exitSyntaxErr   = 100;
    val exitSemanticErr = 200;
    val exitInvalidOptions = 2;

    var fullTypeCheck = true
    var compile = false
    var optimise = false
    var enableColours = false
}