package redscript.lang

import java.lang.reflect.InvocationTargetException

class RedJavaObject(val obj: AnyRef) extends RedObject
{
    override def __str__ : String = s"<JavaObject `$obj`>"
    override def __repr__ : String = s"<JavaObject `$obj`>"

    override def __hash__ : Int = obj.hashCode
    override def __eq__(other: RedObject): RedObject = other match
    {
        case v: RedJavaObject => RedBoolean(v.obj.equals(obj))
        case _                => RedBoolean.False
    }

    override def __dir__ : RedTuple =
    {
        val fields = obj.getClass.getFields map (_.getName) map RedString.apply
        val methods = obj.getClass.getMethods map (_.getName) map RedString.apply
        new RedTuple(fields ++ methods)
    }

    override def __getattr__(name: String): RedObject =
    {
        if (obj.getClass.getFields exists (_.getName == name))
            return RedObject.wrapObject(obj.getClass.getField(name).get(obj))

        val methods = obj.getClass.getMethods filter (_.getName == name)

        if (methods.isEmpty)
            return super.__getattr__(name)

        if (methods.length > 1 || methods.head.getParameterCount > 0)
            return new RedCallable(name, obj, methods)

        try
        {
            RedObject.wrapObject(methods.head.invoke(obj))
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
