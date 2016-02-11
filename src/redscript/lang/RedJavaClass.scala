package redscript.lang

import java.lang.reflect.InvocationTargetException

class RedJavaClass(val cls: Class[_]) extends RedObject
{
    override def __str__ : String = s"<JavaClass `${cls.toString}`>"
    override def __repr__ : String = s"<JavaClass `${cls.toString}`>"

    override def __dir__ : RedTuple =
    {
        val fields = cls.getFields map (_.getName) map (new RedString(_))
        val methods = cls.getMethods map (_.getName) map (new RedString(_))
        new RedTuple(fields ++ methods)
    }

    override def __getattr__(name: String): RedObject =
    {
        if (cls.getFields exists (_.getName == name))
        {
            return cls.getField(name).get(cls) match
            {
                case field: java.lang.Byte      => new RedInt(field.longValue)
                case field: java.lang.Long      => new RedInt(field.longValue)
                case field: java.lang.Short     => new RedInt(field.longValue)
                case field: java.lang.Integer   => new RedInt(field.longValue)

                case field: java.lang.Float     => new RedFloat(field.floatValue)
                case field: java.lang.Double    => new RedFloat(field.doubleValue)

                case field: java.lang.String    => new RedString(field)
                case field: java.lang.Character => new RedString(field.toString)
                case field: java.lang.Throwable => new RedException(field)

                case field: Class[_]            => new RedJavaClass(field)
                case field                      => new RedJavaObject(field)
            }
        }

        val methods = cls.getMethods filter (_.getName == name)

        if (methods.isEmpty)
            return super.__getattr__(name)

        if (methods.length > 1 || methods.head.getParameterCount > 0)
            return new RedCallable(name, cls, methods)

        try
        {
            RedCallable.wrapObject(methods.head.invoke(cls))
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
