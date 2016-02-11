package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeClass(name: Identifier, parent: Option[List[Identifier]], intfs: Option[List[List[Identifier]]], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
