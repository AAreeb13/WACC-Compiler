package wacc

import semanticChecker.SemanticInfo

object codeGenerator {
/*
 * This function takes in the AST, function and symbol table and generates the assembly code 
 * based on the specified target achitecture
 * 
 * @param semanticResult SemanticInfo contains the AST, function and symbol table
 * codeGenerator is only used when semantic check is successful, so SemanticInfo is always present
 * @param assemblyTarget Defines the target architecture for the code generation
 * @return The asm code generate
 */
    def generateAssembly(semanticResult: Either[String, SemanticInfo], assemblyTarget: TargetConfig): String = {
        val ir = new Translator(semanticResult.toOption.get, assemblyTarget).translate()

        assemblyTarget.assemble(ir)
    }
}