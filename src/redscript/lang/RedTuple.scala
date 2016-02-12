package redscript.lang

class RedTuple(val items: Array[RedObject]) extends RedObject
{
    private class Iterator(tuple: RedTuple) extends RedObject
    {
        private val iter = tuple.items.iterator
        override def __next__ : RedObject = if (iter.hasNext) iter.next else throw new StopIteration
    }

    override def __len__  : Long      = items.length
    override def __bool__ : Boolean   = items.nonEmpty

    override def __iter__ : RedObject = new Iterator(this)
    override def __contains__(item: RedObject): Boolean = items.contains(item)

    override def __str__  : String = s"(${items map (_.__repr__) mkString ", "})"
    override def __repr__ : String = s"(${items map (_.__repr__) mkString ", "})"

    override def __hash__ : Int = items.isEmpty match
    {
        case true  => 0
        case false => items map (_.__hash__) reduce (_ ^ _)
    }

    override def __eq__(other: RedObject): RedObject = other match
    {
        case v: RedTuple => v.__len__ == __len__ match
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

    override def __getitem__(name: RedObject): RedObject = name match
    {
        case v: RedInt =>
            if (v.value < -items.length || v.value >= items.length)
                throw new IndexError(s"Tuple index out of bound ${v.value}")

            if (v.value >= 0)
                items(v.value.toInt)
            else
                items(v.value.toInt + items.length)

        case _ => throw new ValueError("Tuple index must be integers")
    }
}
