package redscript.lang

import scala.math.pow

class RedFloat(val value: Double) extends RedObject
{
    override def __str__ : String = value.toString
    override def __repr__ : String = value.toString

    override def __hash__ : Int = value.hashCode
    override def __bool__ : Boolean = value != 0

    override def __pos__      : RedObject = new RedFloat(+value)
    override def __neg__      : RedObject = new RedFloat(-value)
    override def __bool_not__ : RedObject = RedBoolean(value == 0)

    private def applyCmp(other: RedObject)(operator: => (Double, Double) => Boolean): RedBoolean = RedBoolean(other match
    {
        case x: RedInt     => operator(value, x.value)
        case x: RedFloat   => operator(value, x.value)
        case x: RedBoolean => operator(value, if (x.value) 1 else 0)
        case RedNull.Null  => operator(value, 0.0)
        case _             => throw new TypeError(s"${other.getClass.getName} is not comparable with Float")
    })

    private def applyFloat(other: RedObject)(operator: => (Double, Double) => Double): RedObject = new RedFloat(operator(value, other match
    {
        case x: RedInt     => x.value
        case x: RedFloat   => x.value
        case x: RedBoolean => if (x.value) 1.0 else 0.0
        case RedNull.Null  => 0.0
        case _             => throw new TypeError(s"${other.getClass.getName} canot be coerced with Float")
    }))

    override def __bool_or__(other: RedObject) : RedObject = RedBoolean((value != 0) || other.__bool__)
    override def __bool_and__(other: RedObject): RedObject = RedBoolean((value != 0) && other.__bool__)
    override def __bool_xor__(other: RedObject): RedObject = RedBoolean((value == 0) == other.__bool__)

    override def __eq__(other: RedObject) : RedObject = applyCmp(other)(_ == _)
    override def __le__(other: RedObject) : RedObject = applyCmp(other)(_ <  _)
    override def __ge__(other: RedObject) : RedObject = applyCmp(other)(_ >  _)
    override def __neq__(other: RedObject): RedObject = applyCmp(other)(_ != _)
    override def __leq__(other: RedObject): RedObject = applyCmp(other)(_ <= _)
    override def __geq__(other: RedObject): RedObject = applyCmp(other)(_ >= _)

    override def __pow__(other: RedObject): RedObject = applyFloat(other)(pow)
    override def __add__(other: RedObject): RedObject = applyFloat(other)(_ + _)
    override def __sub__(other: RedObject): RedObject = applyFloat(other)(_ - _)
    override def __mul__(other: RedObject): RedObject = applyFloat(other)(_ * _)
    override def __div__(other: RedObject): RedObject = applyFloat(other)(_ / _)
    override def __mod__(other: RedObject): RedObject = applyFloat(other)(_ % _)

    override def __inc_pow__(other: RedObject): RedObject = applyFloat(other)(pow)
    override def __inc_add__(other: RedObject): RedObject = applyFloat(other)(_ + _)
    override def __inc_sub__(other: RedObject): RedObject = applyFloat(other)(_ - _)
    override def __inc_mul__(other: RedObject): RedObject = applyFloat(other)(_ * _)
    override def __inc_div__(other: RedObject): RedObject = applyFloat(other)(_ / _)
    override def __inc_mod__(other: RedObject): RedObject = applyFloat(other)(_ % _)
}
