package redscript.lang

class RedFunction extends RedObject
{
    override def __str__  : String = "<RedFunction>"
    override def __repr__ : String = "<RedFunction>"
    override def __invoke__(args: Array[RedObject]): RedObject = throw new NotImplementedError("Function not implemented")
}
