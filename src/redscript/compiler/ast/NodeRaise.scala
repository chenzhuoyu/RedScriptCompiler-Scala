package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeRaise(expr: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
