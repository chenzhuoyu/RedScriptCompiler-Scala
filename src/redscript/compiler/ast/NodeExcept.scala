package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeCatch(val names: List[Identifier], val name: Option[Identifier]) {}
class NodeExcept(types: List[NodeCatch], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
