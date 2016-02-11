package redscript.lang

import java.lang.reflect.{InvocationTargetException, Method}

import scala.language.postfixOps

class RedCallable(val name: String, val self: AnyRef, val methods: Array[Method]) extends RedObject
{
    override def __str__ : String = s"<Method `${self.getClass.getName}.$name`>"
    override def __repr__ : String = s"<Method `${self.getClass.getName}.$name`>"

    private def convertArgs(method: Method, args: Array[RedObject]): Array[AnyRef] =
    {
        val params = new Array[AnyRef](args.length)

        for (((actual, formal), index) <- args zip method.getParameterTypes zipWithIndex) formal match
        {
            case java.lang.Long.TYPE => actual match
            {
                case v: RedInt => params(index) = v.value: java.lang.Long
                case _         => return null
            }

            case java.lang.Double.TYPE => actual match
            {
                case v: RedFloat => params(index) = v.value: java.lang.Double
                case _           => return null
            }

            case java.lang.Boolean.TYPE => actual match
            {
                case v: RedBoolean => params(index) = v.value: java.lang.Boolean
                case _             => return null
            }

            case java.lang.Byte.TYPE => actual match
            {
                case v: RedInt if v.value >= Byte.MinValue && v.value <= Byte.MaxValue => params(index) = v.value.toByte: java.lang.Byte
                case _                                                                 => return null
            }

            case java.lang.Short.TYPE => actual match
            {
                case v: RedInt if v.value >= Short.MinValue && v.value <= Short.MaxValue => params(index) = v.value.toShort: java.lang.Short
                case _                                                                   => return null
            }

            case java.lang.Integer.TYPE => actual match
            {
                case v: RedInt if v.value >= Int.MinValue && v.value <= Int.MaxValue => params(index) = v.value.toInt: java.lang.Integer
                case _                                                               => return null
            }

            case java.lang.Float.TYPE => actual match
            {
                case v: RedFloat if v.value >= Float.MinValue && v.value <= Float.MaxValue => params(index) = v.value.toFloat: java.lang.Float
                case _                                                                     => return null
            }

            case java.lang.Character.TYPE => actual match
            {
                case v: RedString if v.value.length == 1 => params(index) = v.value.head: java.lang.Character
                case _                                   => return null
            }

            case _ => actual match
            {
                case v: RedInt        => params(index) = v.value: java.lang.Long
                case v: RedFloat      => params(index) = v.value: java.lang.Double

                case v: RedTuple      => params(index) = v.items
                case v: RedArray      => params(index) = v.items.toArray
                case v: RedString     => params(index) = v.value
                case v: RedException  => params(index) = v.reason

                case v: RedJavaClass  => params(index) = v.cls
                case v: RedJavaObject => params(index) = v.obj

                case RedNull.Null     => params(index) = null
                case RedBoolean.True  => params(index) = true: java.lang.Boolean
                case RedBoolean.False => params(index) = false: java.lang.Boolean
                case v                => params(index) = v
            }
        }

        params
    }

    override def __invoke__(args: Array[RedObject]): RedObject =
    {
        var callee: Method = null
        var params: Array[AnyRef] = null

        for (method <- methods filter (_.getParameterCount == args.length))
        {
            val actual = convertArgs(method, args)

            if (actual != null && params != null)
                throw new ArgumentError(s"Call to `$name` with such actual parameter is ambiguous")

            callee = method
            params = actual
        }

        if (params == null)
            throw new ArgumentError(s"Cannot find method `$name` with such signature")

        try
        {
            println(self)
            RedCallable.wrapObject(callee.invoke(self, params: _*))
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    }
}

object RedCallable
{
    def wrapObject(obj: AnyRef): RedObject = obj match
    {
        case null                     => RedNull.Null

        case v: java.lang.Byte        => new RedInt(v.longValue)
        case v: java.lang.Long        => new RedInt(v.longValue)
        case v: java.lang.Short       => new RedInt(v.longValue)
        case v: java.lang.Integer     => new RedInt(v.longValue)

        case v: java.lang.Float       => new RedFloat(v.doubleValue)
        case v: java.lang.Double      => new RedFloat(v.doubleValue)

        case v: java.lang.String      => new RedString(v)
        case v: java.lang.Throwable   => new RedException(v)

        case v: RedObject             => v
        case v: java.lang.Boolean     => RedBoolean(v)
        case v if v.getClass.isArray  => new RedTuple(v.asInstanceOf[Array[AnyRef]] map wrapObject)
        case v: Class[_]              => new RedJavaClass(v)
        case v                        => new RedJavaObject(v)
    }
}
