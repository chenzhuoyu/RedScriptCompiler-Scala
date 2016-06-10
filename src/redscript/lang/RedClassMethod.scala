package redscript.lang

import java.lang.invoke.{MethodHandles, MethodType}
import java.lang.reflect.{InvocationTargetException, Method}

import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class RedClassMethod(val name: String, val cls: Class[RedObject], val methods: Array[Method]) extends RedObject
{
    override def __str__ : String = s"<ClassMethod `${cls.getClass.getName}.$name`>"
    override def __repr__ : String = s"<ClassMethod `${cls.getClass.getName}.$name`>"

    override def __invoke__(args: Array[RedObject]): RedObject =
    {
        val self = args.head
        val params = args drop 1
        val results = ArrayBuffer[(Method, Array[AnyRef], Long)]()
        val candidates = methods filter (_.getParameterCount == params.length)

        candidates foreach { method => RedObject.convertArguments(method, params) match
        {
            case (actual, score) => if (actual != null)
                results.append((method, actual, score))
        }}

        if (results.isEmpty)
            throw new ArgumentError(s"Cannot find method `$name` with such signature")

        try
        {
            results sortBy (_._3) last match
            {
                case (method, actual, score) =>
                    val ptypes = actual.map(_.getClass).asInstanceOf[Array[java.lang.Class[_]]]
                    val lookupField = classOf[MethodHandles.Lookup].getDeclaredField("IMPL_LOOKUP")

                    /* FIXME: this is a hack to Java Runtime, fix it later */
                    lookupField.setAccessible(true)

                    val lookup = lookupField.get(null).asInstanceOf[MethodHandles.Lookup]
                    val target = lookup.findSpecial(cls, name, MethodType.methodType(method.getReturnType, ptypes), self.getClass)

                    RedObject.wrapObject(target.invokeWithArguments(seqAsJavaList(List(self) ++ actual)))
            }
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
