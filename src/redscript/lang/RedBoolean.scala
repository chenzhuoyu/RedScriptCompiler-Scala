package redscript.lang

class RedBoolean private(val value: Boolean) extends RedObject
{
    override def __str__ : String = value.toString
    override def __repr__ : String = value.toString

    override def __bool__     : Boolean = value
    override def __bool_not__ : RedObject = new RedBoolean(!value)

    override def __bool_or__ (other: RedObject): RedObject = new RedBoolean(value || other.__bool__)
    override def __bool_and__(other: RedObject): RedObject = new RedBoolean(value && other.__bool__)
    override def __bool_xor__(other: RedObject): RedObject = new RedBoolean(value != other.__bool__)
}

object RedBoolean
{
    final val True = new RedBoolean(true)
    final val False = new RedBoolean(false)

    def apply(value: Boolean) = if (value) True else False
}
