package wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._

object lexer {
    private val desc = LexicalDesc.plain.copy(
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
            identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_')
        ),
        spaceDesc = SpaceDesc.plain, 
        symbolDesc = SymbolDesc.plain.copy (
            hardKeywords = Set (
                "int",
                "bool",
                "char",
                "string",
                "pair",
                "len",
                "ord",
                "chr",
                "true",
                "false",
                "null",
                "skip",
                "read",
                "free",
                "return",
                "exit",
                "print",
                "println",
                "if",
                "then",
                "else",
                "fi",
                "while",
                "do",
                "done",
                "begin",
                "end",
                "is",
                "fst",
                "snd"
            ),
            hardOperators = Set (
                // binary operators
                "*",
                "/",
                "%",
                "+",
                "-",
                ">",
                ">=",
                "<",
                "<=",
                "==",
                "!=",
                "&&",
                "||",
                // unary operators
                "!",
                "-"
            )
        )
    )
    private val lexer = new Lexer(desc)

    val intLiter: Parsley[BigInt] = lexer.lexeme.integer.decimal32[BigInt]
    val ident: Parsley[String] = lexer.lexeme.names.identifier
    val charLiter = lexer.lexeme.character.ascii
    val strLiter = lexer.lexeme.string.ascii
    val boolLiter = lexer.lexeme.symbol("true").as(true) | lexer.lexeme.symbol("false").as(false)
    val pairLiter = lexer.lexeme.symbol("null")

    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
