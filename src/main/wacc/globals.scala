package wacc

/**
  * Constants that are used throughout the program
  */
object globals {
    val exitSuccess     = 0;
    val exitSyntaxErr   = 100;
    val exitSemanticErr = 200;
    val exitRuntimeErr = 300;
    val exitInvalidOptions = 2;

    var onlySyntaxCheck = false
    var onlyTypeCheck = false
    var fullCompile = true
    var fullOptimise = false
    var execute = false
    var enableColours = false

    var useDocker = false
    var useWSL = false
}