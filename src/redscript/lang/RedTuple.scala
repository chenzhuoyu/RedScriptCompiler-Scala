package redscript.lang

class RedTuple(val items: Array[RedObject]) extends RedObject with Seq[RedObject]
{
    private class Iterator(tuple: RedTuple) extends RedObject
    {
        private val iter = tuple.iterator
        override def __next__ : RedObject = if (iter.hasNext) iter.next else throw new StopIteration
    }

    override def apply(idx: Int): RedObject = items(idx)
    override def length: Int = items.length
    override def iterator: scala.Iterator[RedObject] = items.iterator

    override def __len__  : Long      = items.length
    override def __bool__ : Boolean   = items.nonEmpty
    override def __iter__ : RedObject = new Iterator(this)

    override def __str__  : String = s"(${items map (_.__repr__) mkString ", "})"
    override def __repr__ : String = s"(${items map (_.__repr__) mkString ", "})"

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
