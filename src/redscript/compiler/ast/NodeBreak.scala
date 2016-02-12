package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeBreak extends Node
{
    override def assemble(assembler: Assembler): Unit = assembler.classes.top.method.markBreak()
}
