package redscript.lang

import scala.language.postfixOps
import scala.math.pow

class RedInt private(val value: Long) extends RedObject
{
    override def __str__ : String = value.toString
    override def __repr__ : String = value.toString

    override def __hash__ : Int = value.hashCode
    override def __bool__ : Boolean = value != 0

    override def __pos__      : RedObject = RedInt(+value)
    override def __neg__      : RedObject = RedInt(-value)
    override def __not__      : RedObject = RedInt(~value)
    override def __bool_not__ : RedObject = RedBoolean(value == 0)

    private def applyCmp(other: RedObject)(operator: => (Long, Long) => Boolean): RedBoolean = RedBoolean(other match
    {
        case x: RedInt     => operator(value, x.value)
        case x: RedFloat   => if (x.value % 1 == 0) operator(value, x.value.toLong) else false
        case x: RedBoolean => operator(value, if (x.value) 1 else 0)
        case RedNull.Null  => operator(value, 0)
        case _             => throw new TypeError(s"${other.getClass.getName} is not comparable with Int")
    })

    private def applyInteger(other: RedObject)(op1: => (Long, Long) => Long)(op2: => (Long, Double) => Double): RedObject = other match
    {
        case x: RedInt     => RedInt(op1(value, x.value))
        case x: RedFloat   => RedFloat(op2(value, x.value))
        case x: RedBoolean => RedInt(op1(value, if (x.value) 1 else 0))
        case RedNull.Null  => RedInt(op1(value, 0))
        case _             => throw new TypeError(s"${other.getClass.getName} canot be coerced with Int")
    }

    override def __eq__(other: RedObject) : RedObject = RedBoolean(other match
    {
        case x: RedInt     => value == x.value
        case x: RedFloat   => if (x.value % 1 == 0) value == x.value.toLong else false
        case x: RedBoolean => if (x.value) value == 1 else value == 0
        case RedNull.Null  => value == 0
        case _             => false
    })

    override def __neq__(other: RedObject): RedObject = RedBoolean(other match
    {
        case x: RedInt     => value != x.value
        case x: RedFloat   => if (x.value % 1 == 0) value != x.value.toLong else true
        case x: RedBoolean => if (x.value) value != 1 else value != 0
        case RedNull.Null  => value != 0
        case _             => true
    })

    override def __bool_or__(other: RedObject) : RedObject = RedBoolean((value != 0) || other.__bool__)
    override def __bool_and__(other: RedObject): RedObject = RedBoolean((value != 0) && other.__bool__)
    override def __bool_xor__(other: RedObject): RedObject = RedBoolean((value == 0) == other.__bool__)

    override def __le__(other: RedObject) : RedObject = applyCmp(other)(_ <  _)
    override def __ge__(other: RedObject) : RedObject = applyCmp(other)(_ >  _)
    override def __leq__(other: RedObject): RedObject = applyCmp(other)(_ <= _)
    override def __geq__(other: RedObject): RedObject = applyCmp(other)(_ >= _)

    override def __add__(other: RedObject)    : RedObject = applyInteger(other)(_ + _)(_ + _)
    override def __sub__(other: RedObject)    : RedObject = applyInteger(other)(_ - _)(_ - _)
    override def __mul__(other: RedObject)    : RedObject = applyInteger(other)(_ * _)(_ * _)
    override def __div__(other: RedObject)    : RedObject = applyInteger(other)(_ / _)(_ / _)
    override def __mod__(other: RedObject)    : RedObject = applyInteger(other)(_ % _)(_ % _)
    override def __pow__(other: RedObject)    : RedObject = applyInteger(other)(pow(_, _).toLong)(pow(_, _))
    override def __or__ (other: RedObject)    : RedObject = applyInteger(other)(_ | _)(throw new TypeError("Operator '|' between Int and Float not defined"))
    override def __and__(other: RedObject)    : RedObject = applyInteger(other)(_ & _)(throw new TypeError("Operator '&' between Int and Float not defined"))
    override def __xor__(other: RedObject)    : RedObject = applyInteger(other)(_ ^ _)(throw new TypeError("Operator '^' between Int and Float not defined"))
    override def __lshift__(other: RedObject) : RedObject = applyInteger(other)(_ << _)(throw new TypeError("Operator '<<' between Int and Float not defined"))
    override def __rshift__(other: RedObject) : RedObject = applyInteger(other)(_ >> _)(throw new TypeError("Operator '>>' between Int and Float not defined"))

    override def __inc_add__(other: RedObject)   : RedObject = applyInteger(other)(_ + _)(_ + _)
    override def __inc_sub__(other: RedObject)   : RedObject = applyInteger(other)(_ - _)(_ - _)
    override def __inc_mul__(other: RedObject)   : RedObject = applyInteger(other)(_ * _)(_ * _)
    override def __inc_div__(other: RedObject)   : RedObject = applyInteger(other)(_ / _)(_ / _)
    override def __inc_mod__(other: RedObject)   : RedObject = applyInteger(other)(_ % _)(_ % _)
    override def __inc_pow__(other: RedObject)   : RedObject = applyInteger(other)(pow(_, _).toLong)(pow(_, _))
    override def __inc_or__ (other: RedObject)   : RedObject = applyInteger(other)(_ | _)(throw new TypeError("Operator '|' between Int and Float not defined"))
    override def __inc_and__(other: RedObject)   : RedObject = applyInteger(other)(_ & _)(throw new TypeError("Operator '&' between Int and Float not defined"))
    override def __inc_xor__(other: RedObject)   : RedObject = applyInteger(other)(_ ^ _)(throw new TypeError("Operator '^' between Int and Float not defined"))
    override def __inc_lshift__(other: RedObject): RedObject = applyInteger(other)(_ << _)(throw new TypeError("Operator '<<' between Int and Float not defined"))
    override def __inc_rshift__(other: RedObject): RedObject = applyInteger(other)(_ >> _)(throw new TypeError("Operator '>>' between Int and Float not defined"))
}

object RedInt
{
    val cache: Array[RedInt] = (-256 to 255) map (new RedInt(_)) toArray
    def apply(value: Long): RedInt = if (value >= -256 && value <= 255) cache(value.toInt + 256) else new RedInt(value)
}
