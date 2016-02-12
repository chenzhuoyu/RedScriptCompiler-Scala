package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeContinue extends Node
{
    override def assemble(assembler: Assembler): Unit = assembler.classes.top.method.markContinue()
}
