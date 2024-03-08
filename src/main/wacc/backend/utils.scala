package wacc

object utils {
    def toRaw(s: String): String =
        s.flatMap(_ match {
            case '\u0000' => "\\\u0000"
            case '\\' => "\\\\"
            case '\"' => "\\\""
            case '\n' => "\\n"
            case '\t' => "\\t"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '\b' => "\\b"
            case other => other.toString
        })
}