package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeSelect(expr: NodeExpr, cases: List[NodeCase], default: Option[List[NodeStatement]]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
