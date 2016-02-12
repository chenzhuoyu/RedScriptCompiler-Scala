package redscript.compiler

class SyntaxError(val message: String, val line: Int = -1, val column: Int = -1) extends Exception
{
    override def getMessage: String =
    {
        if (line < 0 || column < 0)
            message
        else
            s"$message at line $line, column $column"
    }
}
