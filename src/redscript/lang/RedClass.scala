package redscript.lang

class RedClass(val name: String, override val cls: Class[_]) extends RedJavaClass(cls)
{
    override def __str__ : String = super.__str__
    override def __repr__ : String = super.__repr__
}
