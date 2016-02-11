package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.Assembler

class NodeFor(name: Identifier, expr: NodeExpr, body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val end = new Label
        val start = new Label
        val variable = assembler.makeLocal(name.value)

        expr.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__iter__", "()Lredscript/lang/RedObject;", false)
        assembler.visitor.visitTryCatchBlock(start, end, end, "redscript/lang/StopIteration")
        assembler.visitor.visitLabel(start)
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__next__", "()Lredscript/lang/RedObject;", false)
        assembler.visitor.visitVarInsn(Opcodes.ASTORE, variable)
        body.foreach(_.assemble(assembler))
        assembler.visitor.visitJumpInsn(Opcodes.GOTO, start)
        assembler.visitor.visitLabel(end)
        assembler.visitor.visitInsn(Opcodes.POP)
    }
}
