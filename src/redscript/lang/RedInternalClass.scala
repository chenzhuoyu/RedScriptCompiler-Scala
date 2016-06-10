package redscript.lang

class RedInternalClass(override val cls: Class[RedObject]) extends RedJavaClass(cls)
{
    override def __getattr__(name: String): RedObject = cls.getMethods filter (_.getName == name) match
    {
        case methods if methods.nonEmpty                         => new RedClassMethod(name, cls, methods)
        case methods if cls.getFields exists (_.getName == name) => RedObject.wrapObject(cls.getField(name).get(cls))
        case _                                                   => super.__getattr__(name)
    }
}
