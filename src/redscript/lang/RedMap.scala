package redscript.lang

import scala.collection.mutable

class RedMap(val items: Array[(RedObject, RedObject)]) extends RedObject
{
    private val storage = mutable.HashMap[RedObject, RedObject](items:_*)
    private class Iterator(map: RedMap) extends RedObject
    {
        private val iter = map.storage.iterator
        override def __next__ : RedObject =
        {
            if (!iter.hasNext)
                throw new StopIteration

            val next = iter.next
            new RedTuple(Array(next._1, next._2))
        }
    }

    override def __len__  : Long      = storage.size
    override def __bool__ : Boolean   = storage.nonEmpty

    override def __hash__ : Int = throw new NotSupportedError("map() is unhashable")
    override def __iter__ : RedObject = new Iterator(this)
    override def __contains__(item: RedObject): Boolean = storage.contains(item)

    override def __str__  : String = s"{${storage map { case (k, v) => s"${k.__repr__}: ${v.__repr__}" } mkString ", "}}"
    override def __repr__ : String = s"{${storage map { case (k, v) => s"${k.__repr__}: ${v.__repr__}" } mkString ", "}}"

    override def __eq__(other: RedObject): RedObject = other match
    {
        case v: RedMap => v.__len__ == __len__ match
        {
            case false => RedBoolean.False
            case true  => v.storage.isEmpty match
            {
                case true  => RedBoolean.True
                case false => storage zip v.storage map { case ((k1, v1), (k2, v2)) => k1.__eq__(k2).__bool_and__(v1.__eq__(v2)) } reduce (_.__bool_and__(_))
            }
        }

        case _ => RedBoolean.False
    }

    override def __getitem__(name: RedObject): RedObject = storage.get(name) match
    {
        case Some(value) => value
        case None        => throw new KeyError(s"Key not found ${name.__repr__}")
    }
}
