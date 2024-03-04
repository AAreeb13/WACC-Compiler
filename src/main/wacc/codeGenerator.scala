package wacc

import semanticChecker.SemanticInfo

object codeGenerator {
    def generateAssembly(semanticResult: Either[String, SemanticInfo], assemblyTarget: TargetConfig): String = {
        val ir = new Translator(semanticResult.toOption.get, assemblyTarget).translate()

        assemblyTarget match {
            case X86Config => X86Generator.assemble(ir)
            case _ => ???
        }
    }
}