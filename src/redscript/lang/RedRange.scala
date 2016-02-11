package redscript.lang

class RedRange(val lower: RedObject, val upper: RedObject) extends RedObject
{
    private class Iterator(private var lower: Long, upper: Long) extends RedObject
    {
        override def __next__ : RedObject =
        {
            if (lower > upper)
                throw new StopIteration()

            lower += 1
            new RedInt(lower - 1)
        }
    }

    override def __str__ : String = "<RedRange>"
    override def __repr__ : String = "<RedRange>"
    override def __contains__(item: RedObject): Boolean = item.__geq__(lower).__bool_and__(item.__leq__(upper)).__bool__

    override def __len__ : Long =
    {
        if (!lower.isInstanceOf[RedInt] || !upper.isInstanceOf[RedInt])
            throw new ValueError("len() of non-integer range makes no sense")
        else
            upper.asInstanceOf[RedInt].value - lower.asInstanceOf[RedInt].value + 1
    }

    override def __iter__ : RedObject =
    {
        if (!lower.isInstanceOf[RedInt] || !upper.isInstanceOf[RedInt])
            throw new ValueError("Iterates over non-integer range makes no sense")
        else
            new Iterator(lower.asInstanceOf[RedInt].value, upper.asInstanceOf[RedInt].value)
    }
}
