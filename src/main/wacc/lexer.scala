package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.errors.ErrorConfig
import parsley.token.errors.Label
import parsley.token.errors.LabelConfig
import parsley.token.predicate

object lexer {
    private val escapedLiterals = Set('\\', '\"', '\'')
    private val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
        identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
        identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_')
      ),
      spaceDesc = SpaceDesc.plain.copy(
        lineCommentStart = "#",
        space = predicate.Basic(c => c == ' ' || c == '\t' || c == '\n' || c == '\r')
      ),
      symbolDesc = SymbolDesc.plain.copy(
        hardKeywords = Set(
          "skip",
          "begin",
          "end",
          "read",
          "is",
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
          "newpair",
          "call",
          "fst",
          "snd",
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
          "null"
        ),
        hardOperators = Set(
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
      ),
      numericDesc = numeric.NumericDesc.plain.copy(
        integerNumbersCanBeHexadecimal = false,
        integerNumbersCanBeOctal = false,
        decimalExponentDesc = numeric.ExponentDesc.NoExponents
      ),
      textDesc = text.TextDesc.plain.copy(
        escapeSequences = text.EscapeDesc.plain.copy(
          escBegin = '\\',
          literals = escapedLiterals,
          mapping = Map(
            "0" -> 0x0,
            "b" -> 0x8,
            "t" -> 0x9,
            "n" -> 0xa,
            "f" -> 0xc,
            "r" -> 0xd
          )
        ),
        graphicCharacter = predicate.Basic(c => c >= ' '.toInt && !escapedLiterals.contains(c))
      )
    )
    private val errConfig = new ErrorConfig {
        override def labelSymbolOperator(symbol: String): LabelConfig =
            Label("arithmetic operator")

        override def labelIntegerUnsignedNumber: LabelConfig =
            Label("number")
    }
    private val lexer = new Lexer(desc, errConfig)

    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    val ident: Parsley[String]      = lexer.lexeme.names.identifier
    val intLiteral: Parsley[BigInt] = lexer.lexeme.integer.decimal32[BigInt] // could also make Long
    val charLiteral                 = lexer.lexeme.character.ascii
    val stringLiteral               = lexer.lexeme.string.ascii
    val pairLiteral                 = lexer.lexeme.symbol("null")
    val boolLiteral                 = lexer.lexeme.symbol("false").as(false) | lexer.lexeme.symbol("true").as(true) 

    val implicits = lexer.lexeme.symbol.implicits
}