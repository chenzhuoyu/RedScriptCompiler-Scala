package redscript.lang

class RedString private(val value: String) extends RedObject
{
    override def __str__ : String = value
    override def __repr__ : String = (value.toList map { _ match
    {
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\r' => "\\r"
        case '\"' => "\\\""
        case '\\' => "\\\\"
        case '\u0000' => "\\0"
        case ch if ' ' <= ch && ch <= '\u007e' => ch.toString
        case ch =>
            val hex = Integer.toHexString(ch.toInt)
            "\\u%s%s".format("0" * (4 - hex.length), hex)
    }}) mkString ("\"", "", "\"")

    override def __hash__ : Int = value.hashCode
}

object RedString
{
    def apply(value: String): RedString = new RedString(value)
}
