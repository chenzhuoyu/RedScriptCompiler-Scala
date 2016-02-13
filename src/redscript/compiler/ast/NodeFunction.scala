package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeArgument(val name: Identifier, val variant: Boolean)
class NodeFunction(name: Identifier, args: Option[List[NodeArgument]], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
