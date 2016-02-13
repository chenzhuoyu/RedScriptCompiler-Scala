package redscript.lang

import java.lang.reflect.InvocationTargetException

class RedJavaClass(val cls: Class[_]) extends RedObject
{
    override def __str__ : String = s"<JavaClass `$cls`>"
    override def __repr__ : String = s"<JavaClass `$cls`>"

    override def __hash__ : Int = cls.hashCode
    override def __eq__(other: RedObject): RedObject = other match
    {
        case v: RedJavaObject => RedBoolean(v.obj.equals(cls))
        case _                => RedBoolean.False
    }

    override def __dir__ : RedTuple =
    {
        val fields = cls.getFields map (_.getName) map RedString.apply
        val methods = cls.getMethods map (_.getName) map RedString.apply
        new RedTuple(fields ++ methods)
    }

    override def __getattr__(name: String): RedObject =
    {
        if (cls.getFields exists (_.getName == name))
            return RedObject.wrapObject(cls.getField(name).get(cls))

        val methods = cls.getMethods filter (_.getName == name)

        if (methods.isEmpty)
            return super.__getattr__(name)

        if (methods.length > 1 || methods.head.getParameterCount > 0)
            return new RedCallable(name, cls, methods)

        try
        {
            RedObject.wrapObject(methods.head.invoke(cls))
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
