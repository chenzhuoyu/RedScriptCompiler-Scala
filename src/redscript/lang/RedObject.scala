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

    def __eq__(other: RedObject) : RedObject = RedBoolean(this == other)
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
        val fields = getClass.getFields map (_.getName) map (new RedString(_))
        val methods = getClass.getMethods map (_.getName) map (new RedString(_))
        new RedTuple(fields ++ methods)
    }

    def __getattr__(name: String): RedObject =
    {
        if (__dict__ contains name)
            return __dict__(name)

        if (getClass.getFields exists (_.getName == name))
        {
            return getClass.getField(name).get(this) match
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

        val methods = getClass.getMethods filter (_.getName == name)

        if (methods.isEmpty)
            throw new AttributeError(s"No such attribute `$name`")

        if (methods.length > 1 || methods.head.getParameterCount > 0)
            new RedCallable(name, this, methods)
        else
            RedCallable.wrapObject(methods.head.invoke(this))
    }

    override def toString = __str__
    override def hashCode = __hash__

    override def equals(obj: scala.Any): Boolean = obj match
    {
        case x: RedObject => __eq__(x).__bool__
        case _            => false
    }
}
