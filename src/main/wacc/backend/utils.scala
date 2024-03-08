package wacc

object utils {
    def toRaw(s: String, escapeDoubleQuotes: Boolean = true): String =
        s.flatMap(_ match {
            case '\u0000' => "\\\u0000"
            case '\\' => "\\\\"
            case '\"' if escapeDoubleQuotes => "\\\""
            case '\n' => "\\n"
            case '\t' => "\\t"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '\b' => "\\b"
            case other => other.toString
        })
}