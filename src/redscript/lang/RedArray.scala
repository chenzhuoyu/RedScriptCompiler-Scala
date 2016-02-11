package redscript.lang

import scala.collection.mutable

class RedArray(val items: mutable.ArrayBuffer[RedObject]) extends RedObject with Seq[RedObject]
{
    private class Iterator(array: RedArray) extends RedObject
    {
        private val iter = array.iterator
        override def __next__ : RedObject = if (iter.hasNext) iter.next else throw new StopIteration
    }

    override def apply(idx: Int): RedObject = items(idx)
    override def length: Int = items.length
    override def iterator: scala.Iterator[RedObject] = items.iterator

    override def __len__  : Long      = items.length
    override def __bool__ : Boolean   = items.nonEmpty
    override def __iter__ : RedObject = new Iterator(this)

    override def __str__  : String = s"[${items map (_.__repr__) mkString ", "}]"
    override def __repr__ : String = s"[${items map (_.__repr__) mkString ", "}]"
}
