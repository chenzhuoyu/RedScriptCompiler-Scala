package redscript.lang

import scala.collection.mutable

class RedMap(val items: Array[(RedObject, RedObject)]) extends RedObject with Seq[RedObject]
{
    private class Iterator(map: RedMap) extends RedObject
    {
        private val iter = map.iterator
        override def __next__ : RedObject = if (iter.hasNext) iter.next else throw new StopIteration
    }

    private class ProxyIterator(map: RedMap) extends scala.Iterator[RedObject]
    {
        private val iter = map.items.iterator
        override def next: RedObject = new RedTuple(iter.next())
        override def hasNext: Boolean = iter.hasNext
    }

    private val storage = mutable.HashMap[RedObject, RedObject](items:_*)

    override def apply(idx: Int): RedObject = new RedTuple(items(idx))
    override def length: Int = items.length
    override def iterator: scala.Iterator[RedObject] = new ProxyIterator(this)

    override def __len__  : Long      = items.length
    override def __bool__ : Boolean   = items.nonEmpty
    override def __iter__ : RedObject = new Iterator(this)

    override def __str__  : String = s"{${items map { case (k, v) => s"${k.__repr__}: ${v.__repr__}" } mkString ", "}}"
    override def __repr__ : String = s"{${items map { case (k, v) => s"${k.__repr__}: ${v.__repr__}" } mkString ", "}}"

    override def __getitem__(name: RedObject): RedObject = storage.get(name) match
    {
        case Some(value) => value
        case None        => throw new KeyError(s"Key not found ${name.__repr__}")
    }
}
