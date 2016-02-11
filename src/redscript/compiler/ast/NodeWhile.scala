package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.Assembler

class NodeWhile(cond: NodeExpr, body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val label = new Label
        val endwhile = new Label

        assembler.visitor.visitLabel(label)
        cond.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__bool__", "()Z", false)
        assembler.visitor.visitJumpInsn(Opcodes.IFEQ, endwhile)
        body.foreach(_.assemble(assembler))
        assembler.visitor.visitJumpInsn(Opcodes.GOTO, label)
        assembler.visitor.visitLabel(endwhile)
    }
}
