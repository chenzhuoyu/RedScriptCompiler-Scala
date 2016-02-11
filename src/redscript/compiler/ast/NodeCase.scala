package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeCase(exprs: List[NodeExpr], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
