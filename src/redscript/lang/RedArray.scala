package redscript.lang

import scala.collection.mutable

class RedArray(val items: mutable.ArrayBuffer[RedObject]) extends RedObject
{
    private class Iterator(array: RedArray) extends RedObject
    {
        private val iter = array.items.iterator
        override def __next__ : RedObject = if (iter.hasNext) iter.next else throw new StopIteration
    }

    override def __len__  : Long      = items.length
    override def __bool__ : Boolean   = items.nonEmpty

    override def __hash__ : Int = throw new NotSupportedError("array() is unhashable")
    override def __iter__ : RedObject = new Iterator(this)
    override def __contains__(item: RedObject): Boolean = items.contains(item)

    override def __str__  : String = s"[${items map (_.__repr__) mkString ", "}]"
    override def __repr__ : String = s"[${items map (_.__repr__) mkString ", "}]"

    override def __eq__(other: RedObject): RedObject = other match
    {
        case v: RedArray => v.__len__ == __len__ match
        {
            case false => RedBoolean.False
            case true  => v.items.isEmpty match
            {
                case true  => RedBoolean.True
                case false => items zip v.items map { case (a, b) => a.__eq__(b) } reduce (_.__bool_and__(_))
            }
        }

        case _ => RedBoolean.False
    }
}
