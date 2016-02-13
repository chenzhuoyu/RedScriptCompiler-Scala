package redscript.lang

import scala.collection.mutable

class RedObject
{
    val __dict__ = mutable.HashMap[String, RedObject]()

    def __len__ : Long     = throw new NotSupportedError(s"__len__() is not supported for class ${getClass.getName}")
    def __bool__ : Boolean = throw new NotSupportedError(s"__bool__() is not supported for class ${getClass.getName}")

    def __init__(args: Array[RedObject]): Unit = ()
    def __invoke__(args: Array[RedObject]): RedObject = throw new NotSupportedError(s"${getClass.getName} is not callable")

    def __str__  : String = s"<${getClass.getName}>"
    def __repr__ : String = s"<${getClass.getName}>"

    def __delattr__(name: String): Unit                  = __dict__.remove(name)
    def __setattr__(name: String, attr: RedObject): Unit = __dict__(name) = attr

    def __delitem__(name: RedObject): Unit                  = throw new NotSupportedError(s"__delitem__() is not supported for class ${getClass.getName}")
    def __getitem__(name: RedObject): RedObject             = throw new NotSupportedError(s"__getitem__() is not supported for class ${getClass.getName}")
    def __setitem__(name: RedObject, item: RedObject): Unit = throw new NotSupportedError(s"__setitem__() is not supported for class ${getClass.getName}")

    def __bool_or__(other: RedObject) : RedObject = RedBoolean(__bool__ || other.__bool__)
    def __bool_and__(other: RedObject): RedObject = RedBoolean(__bool__ && other.__bool__)
    def __bool_xor__(other: RedObject): RedObject = RedBoolean(__bool__ != other.__bool__)
    def __bool_not__                  : RedObject = RedBoolean(!__bool__)

    def __eq__(other: RedObject) : RedObject = RedBoolean(super.equals(other))
    def __neq__(other: RedObject): RedObject = __eq__(other).__bool_not__

    def __le__(other: RedObject) : RedObject = throw new NotSupportedError(s"__le__() is not supported for class ${getClass.getName}")
    def __ge__(other: RedObject) : RedObject = throw new NotSupportedError(s"__ge__() is not supported for class ${getClass.getName}")
    def __leq__(other: RedObject): RedObject = throw new NotSupportedError(s"__leq__() is not supported for class ${getClass.getName}")
    def __geq__(other: RedObject): RedObject = throw new NotSupportedError(s"__geq__() is not supported for class ${getClass.getName}")

    def __iter__ : RedObject = throw new NotSupportedError(s"__iter__() is not supported for class ${getClass.getName}")
    def __next__ : RedObject = throw new NotSupportedError(s"__next__() is not supported for class ${getClass.getName}")

    def __hash__ : Int = super.hashCode
    def __contains__(item: RedObject): Boolean = __eq__(item).__bool__

    def __pos__ : RedObject = throw new NotSupportedError(s"__pos__() is not supported for class ${getClass.getName}")
    def __neg__ : RedObject = throw new NotSupportedError(s"__neg__() is not supported for class ${getClass.getName}")

    def __add__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__add__() is not supported for class ${getClass.getName}")
    def __sub__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__sub__() is not supported for class ${getClass.getName}")
    def __mul__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__mul__() is not supported for class ${getClass.getName}")
    def __div__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__div__() is not supported for class ${getClass.getName}")
    def __mod__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__mod__() is not supported for class ${getClass.getName}")
    def __pow__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__pow__() is not supported for class ${getClass.getName}")
    def __or__ (other: RedObject)   : RedObject = throw new NotSupportedError(s"__or__() is not supported for class ${getClass.getName}")
    def __and__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__and__() is not supported for class ${getClass.getName}")
    def __xor__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__xor__() is not supported for class ${getClass.getName}")
    def __not__                     : RedObject = throw new NotSupportedError(s"__not__() is not supported for class ${getClass.getName}")
    def __lshift__(other: RedObject): RedObject = throw new NotSupportedError(s"__lshift__() is not supported for class ${getClass.getName}")
    def __rshift__(other: RedObject): RedObject = throw new NotSupportedError(s"__rshift__() is not supported for class ${getClass.getName}")

    def __inc_add__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_add__() is not supported for class ${getClass.getName}")
    def __inc_sub__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_sub__() is not supported for class ${getClass.getName}")
    def __inc_mul__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_mul__() is not supported for class ${getClass.getName}")
    def __inc_div__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_div__() is not supported for class ${getClass.getName}")
    def __inc_mod__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_mod__() is not supported for class ${getClass.getName}")
    def __inc_pow__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_pow__() is not supported for class ${getClass.getName}")
    def __inc_or__ (other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_or__() is not supported for class ${getClass.getName}")
    def __inc_and__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_and__() is not supported for class ${getClass.getName}")
    def __inc_xor__(other: RedObject)   : RedObject = throw new NotSupportedError(s"__inc_xor__() is not supported for class ${getClass.getName}")
    def __inc_lshift__(other: RedObject): RedObject = throw new NotSupportedError(s"__inc_lshift__() is not supported for class ${getClass.getName}")
    def __inc_rshift__(other: RedObject): RedObject = throw new NotSupportedError(s"__inc_rshift__() is not supported for class ${getClass.getName}")

    def __dir__ : RedTuple =
    {
        val fields = getClass.getFields map (_.getName) map RedString.apply
        val methods = getClass.getMethods map (_.getName) map RedString.apply
        new RedTuple(fields ++ methods)
    }

    def __getattr__(name: String): RedObject =
    {
        if (__dict__ contains name)
            return __dict__(name)

        if (getClass.getFields exists (_.getName == name))
            return RedObject.wrapObject(getClass.getField(name).get(this))

        val methods = getClass.getMethods filter (_.getName == name)

        if (methods.isEmpty)
            throw new AttributeError(s"No such attribute `$name`")

        if (methods.length > 1 || methods.head.getParameterCount > 0)
            new RedCallable(name, this, methods)
        else
            RedObject.wrapObject(methods.head.invoke(this))
    }

    override def toString = __str__
    override def hashCode = __hash__

    override def equals(obj: scala.Any): Boolean = obj match
    {
        case x: RedObject => __eq__(x).__bool__
        case _            => false
    }
}

object RedObject
{
    def wrapObject(obj: AnyRef): RedObject = obj match
    {
        case null                     => RedNull.Null
        case java.lang.Boolean.TRUE   => RedBoolean.True
        case java.lang.Boolean.FALSE  => RedBoolean.False

        case v: java.lang.Byte        => RedInt(v.longValue)
        case v: java.lang.Long        => RedInt(v.longValue)
        case v: java.lang.Short       => RedInt(v.longValue)
        case v: java.lang.Integer     => RedInt(v.longValue)

        case v: java.lang.Float       => RedFloat(v.doubleValue)
        case v: java.lang.Double      => RedFloat(v.doubleValue)

        case v: java.lang.String      => RedString(v)
        case v: java.lang.Character   => RedString(v.toString)
        case v: java.lang.Throwable   => RedException(v)

        case v: RedObject             => v
        case v if v.getClass.isArray  => new RedTuple(v.asInstanceOf[Array[AnyRef]] map wrapObject)
        case v: Class[_]              => new RedJavaClass(v)
        case v                        => new RedJavaObject(v)
    }

    def convertObject(value: RedObject, target: Class[_]): Option[(AnyRef, Int)] = target match
    {
        case java.lang.Byte.TYPE => value match
        {
            case v: RedInt if v.value >= Byte.MinValue && v.value <= Byte.MaxValue => Some((java.lang.Byte.valueOf(v.value.toByte), 9))
            case _                                                                 => None
        }

        case java.lang.Short.TYPE => value match
        {
            case v: RedInt if v.value >= Short.MinValue && v.value <= Short.MaxValue => Some((java.lang.Short.valueOf(v.value.toShort), 9))
            case _                                                                   => None
        }

        case java.lang.Integer.TYPE => value match
        {
            case v: RedInt if v.value >= Int.MinValue && v.value <= Int.MaxValue => Some((java.lang.Integer.valueOf(v.value.toInt), 9))
            case _                                                               => None
        }

        case java.lang.Long.TYPE => value match
        {
            case v: RedInt => Some((java.lang.Long.valueOf(v.value), 0))
            case _         => None
        }

        case java.lang.Float.TYPE => value match
        {
            case v: RedInt                                                             => Some((java.lang.Float.valueOf(v.value), 5))
            case v: RedFloat if v.value >= Float.MinValue && v.value <= Float.MaxValue => Some((java.lang.Float.valueOf(v.value.toFloat), 9))
            case _                                                                     => None
        }

        case java.lang.Double.TYPE => value match
        {
            case v: RedInt   => Some((java.lang.Double.valueOf(v.value), 5))
            case v: RedFloat => Some((java.lang.Double.valueOf(v.value), 9))
            case _           => None
        }

        case java.lang.Boolean.TYPE => value match
        {
            case RedBoolean.True  => Some((java.lang.Boolean.TRUE, 9))
            case RedBoolean.False => Some((java.lang.Boolean.FALSE, 9))
            case _                => None
        }

        case java.lang.Character.TYPE => value match
        {
            case v: RedString if v.value.length == 1 => Some((java.lang.Character.valueOf(v.value.head), 9))
            case _                                   => None
        }

        case v if v == classOf[Class[_]] => value match
        {
            case v: RedJavaClass => Some((v.cls, 9))
            case _               => None
        }

        case v if v == classOf[RedObject] => value match
        {
            case v: RedObject => Some((v, 9))
            case _            => None
        }

        case v if v == classOf[java.lang.String] => value match
        {
            case v: RedString => Some((v.value, 9))
            case _            => None
        }

        case v if v == classOf[java.lang.Object] => value match
        {
            case v: RedJavaClass  => Some((v.cls, 5))
            case v: RedJavaObject => Some((v.obj, 9))
            case _                => Some((value, 1))
        }

        case _ => value match
        {
            case v: RedJavaClass  if target.isAssignableFrom(v.cls.getClass) => Some((v.cls, 9))
            case v: RedJavaObject if target.isAssignableFrom(v.obj.getClass) => Some((v.obj, 9))
            case _                if target.isAssignableFrom(value.getClass) => Some((value, 3))
            case _                                                           => None
        }
    }
}
