package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.Assembler

class NodeIf(expr: NodeExpr, success: List[NodeStatement], failed: Option[List[NodeStatement]]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val endif = new Label
        val label = new Label

        expr.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__bool__", "()Z", false)
        assembler.visitor.visitJumpInsn(Opcodes.IFEQ, label)
        success.foreach(_.assemble(assembler))

        failed match
        {
            case None =>
                assembler.visitor.visitLabel(label)

            case Some(stmts) =>
                assembler.visitor.visitJumpInsn(Opcodes.GOTO, endif)
                assembler.visitor.visitLabel(label)
                stmts.foreach(_.assemble(assembler))
                assembler.visitor.visitLabel(endif)
        }
    }
}
