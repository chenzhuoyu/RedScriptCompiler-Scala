package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeDecorator(expr: NodeExpr, func: Either[NodeFunction, NodeDecorator]) extends Node
{
    override def assemble(assembler: Assembler): Unit = ()
}
