package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeRange(lower: NodeExpr, upper: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedRange")
        assembler.visitor.visitInsn(Opcodes.DUP)
        lower.assemble(assembler)
        upper.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedRange", "<init>", "(Lredscript/lang/RedObject;Lredscript/lang/RedObject;)V", false)
    }
}
