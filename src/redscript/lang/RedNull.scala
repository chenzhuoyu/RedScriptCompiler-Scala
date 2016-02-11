package redscript.lang

class RedNull private () extends RedObject
{
    override def __str__ : String = "null"
    override def __repr__ : String = "null"
    override def __bool__ : Boolean = false
}

object RedNull
{
    final val Null = new RedNull
}
