package redscript.lang

class RedException(val reason: Throwable) extends RedObject
class RedScriptException(val message: String) extends Exception(message)

class SystemExit extends RedScriptException("System exit")
class StopIteration extends RedScriptException("Stop iteration")

class KeyError(message: String) extends RedScriptException(message)
class TypeError(message: String) extends RedScriptException(message)
class IndexError(message: String) extends RedScriptException(message)
class ValueError(message: String) extends RedScriptException(message)
class ArgumentError(message: String) extends RedScriptException(message)
class AttributeError(message: String) extends RedScriptException(message)
class NotSupportedError(message: String) extends RedScriptException(message)
