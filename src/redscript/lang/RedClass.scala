package redscript.lang

class RedClass(val name: String, override val cls: Class[_]) extends RedJavaClass(cls)
{
    private val ctor = cls.getConstructor(classOf[Array[RedObject]])
    override def __str__ : String = s"<class `$name`>"
    override def __repr__ : String = s"<class `$name`>"
    override def __invoke__(args: Array[RedObject]): RedObject = ctor.newInstance(args).asInstanceOf[RedObject]
}

object RedClass
{
    def isRedClass(desc: String): Boolean = try
    {
        classOf[RedObject].isAssignableFrom(Class.forName(desc.replace("/", ".")))
    } catch
    {
        case _: ClassNotFoundException => false
    }
}
