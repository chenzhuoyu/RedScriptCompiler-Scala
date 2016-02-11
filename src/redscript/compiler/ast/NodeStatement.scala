package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeStatement(node: Node) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        node.assemble(assembler)
        node match
        {
            case _: NodeValue => assembler.visitor.visitInsn(Opcodes.POP)
            case _ =>
        }
    }
}
