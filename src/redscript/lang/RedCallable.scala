package redscript.lang

import java.lang.reflect.{InvocationTargetException, Method}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class RedCallable(val name: String, val self: AnyRef, val methods: Array[Method]) extends RedObject
{
    override def __str__ : String = s"<Method `${self.getClass.getName}.$name`>"
    override def __repr__ : String = s"<Method `${self.getClass.getName}.$name`>"

    private def convertArgs(method: Method, args: Array[RedObject]): (Array[AnyRef], Long) =
    {
        var score = 0L
        val params = new Array[AnyRef](args.length)

        for (((actual, formal), index) <- args zip method.getParameterTypes zipWithIndex) RedObject.convertObject(actual, formal) match
        {
            case None                   => return (null, -1)
            case Some((result, factor)) =>
                score *= 10
                score += factor
                params(index) = result
        }

        (params, score)
    }

    override def __invoke__(args: Array[RedObject]): RedObject =
    {
        val results = ArrayBuffer[(Method, Array[AnyRef], Long)]()
        val candidates = methods filter (_.getParameterCount == args.length)

        candidates foreach { method => convertArgs(method, args) match
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
                    RedObject.wrapObject(method.invoke(self, actual: _*))
            }
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}
