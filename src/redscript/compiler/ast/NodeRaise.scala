package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeRaise(expr: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        expr.assemble(assembler)
        assembler.visitor.visitTypeInsn(Opcodes.CHECKCAST, "redscript/lang/RedException")
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedException", "reason", "()Ljava/lang/Throwable;", false)
        assembler.visitor.visitInsn(Opcodes.ATHROW)
    }
}
