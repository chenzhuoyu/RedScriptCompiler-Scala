package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeTry(body: List[NodeStatement], excepts: List[NodeExcept], cleanup: Option[List[NodeStatement]]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
