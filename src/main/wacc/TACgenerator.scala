package wacc

import TAC._
import semanticChecker.SemanticInfo
import ast._

class TACgenerator(semanticResult: Either[String, SemanticInfo]) {

    val semanticInfo = semanticResult.toOption.get


    toTAC(semanticInfo.ast)._1


    def toTAC(node: ast.Node): (List[TAC], RegisterTAC) = {
        ???
    }
}