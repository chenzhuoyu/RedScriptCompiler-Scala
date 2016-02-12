package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.Assembler

class NodeFor(name: Identifier, expr: NodeExpr, body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val end = new Label
        val start = new Label
        val handler = new Label
        val variable = assembler.makeLocal(name.value)

        expr.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__iter__", "()Lredscript/lang/RedObject;", false)
        assembler.visitor.visitTryCatchBlock(start, end, handler, "redscript/lang/StopIteration")
        assembler.visitor.visitLabel(start)
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__next__", "()Lredscript/lang/RedObject;", false)
        assembler.visitor.visitVarInsn(Opcodes.ASTORE, variable)
        assembler.visitor.visitLabel(end)
        body.foreach(_.assemble(assembler))
        assembler.visitor.visitJumpInsn(Opcodes.GOTO, start)
        assembler.visitor.visitLabel(handler)
        assembler.visitor.visitInsn(Opcodes.POP)
    }
}
