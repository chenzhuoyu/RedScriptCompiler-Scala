package redscript.lang

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
        val fields = obj.getClass.getFields map (_.getName)
        val methods = obj.getClass.getMethods map (_.getName)
        new RedTuple((fields ++ methods).distinct map RedString.apply)
    }

    override def __getattr__(name: String): RedObject = obj.getClass.getMethods filter (_.getName == name) match
    {
        case methods if methods.nonEmpty                                  => new RedCallable(name, obj, methods)
        case methods if obj.getClass.getFields exists (_.getName == name) => RedObject.wrapObject(obj.getClass.getField(name).get(obj))
        case _                                                            => super.__getattr__(name)
    }
}
