package redscript.lang

import java.lang.reflect.{Constructor, InvocationTargetException}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

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
        val fields = cls.getFields map (_.getName)
        val methods = cls.getMethods map (_.getName)
        new RedTuple((fields ++ methods).distinct map RedString.apply)
    }

    override def __getattr__(name: String): RedObject = cls.getMethods filter (_.getName == name) match
    {
        case methods if methods.nonEmpty                         => new RedCallable(name, cls, methods)
        case methods if cls.getFields exists (_.getName == name) => RedObject.wrapObject(cls.getField(name).get(cls))
        case _                                                   => super.__getattr__(name)
    }

    override def __invoke__(args: Array[RedObject]): RedObject =
    {
        val results = ArrayBuffer[(Constructor[_], Array[AnyRef], Long)]()
        val candidates = cls.getConstructors filter (_.getParameterCount == args.length)

        candidates foreach { method => RedObject.convertArguments(method, args) match
        {
            case (actual, score) => if (actual != null)
                results.append((method, actual, score))
        }}

        if (results.isEmpty)
            throw new ArgumentError(s"Cannot create instance of `${cls.getName}` with such signature")

        try
        {
            results sortBy (_._3) last match
            {
                case (method, actual, score) =>
                    RedObject.wrapObject(method.newInstance(actual: _*).asInstanceOf[AnyRef])
            }
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
