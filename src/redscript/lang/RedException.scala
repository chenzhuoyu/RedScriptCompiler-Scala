package redscript.lang

class RedScriptException(val message: String) extends Exception(message)

class SystemExit extends RedScriptException("System exit")
class StopIteration extends RedScriptException("Stop iteration")

class KeyError(message: String) extends RedScriptException(message)
class TypeError(message: String) extends RedScriptException(message)
class IndexError(message: String) extends RedScriptException(message)
class ValueError(message: String) extends RedScriptException(message)
class ArgumentError(message: String) extends RedScriptException(message)
class SemanticError(message: String) extends RedScriptException(message)
class AttributeError(message: String) extends RedScriptException(message)
class NotSupportedError(message: String) extends RedScriptException(message)

class RedException private(val reason: Throwable) extends RedJavaObject(reason)
{
    override def __str__ : String = s"<Exception `$obj`>"
    override def __repr__ : String = s"<Exception `$obj`>"
}

object RedException
{
    def apply(reason: Throwable): RedException = new RedException(reason)
}
