package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.{SyntaxError, Assembler}

class NodeParallels(targets: List[NodeLValue], exprs: List[NodeExpr], isTuple: Boolean) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        targets foreach { target => target.value.value match
        {
            case v: NodeName =>
                if (v.name.value == "self" && target.value.modifiers.isEmpty)
                    throw new SyntaxError("Re-assignment to `self`", v.name.pos.line, v.name.pos.column)

            case _ =>
        }}

        if (isTuple)
        {
            exprs.foreach(_.assemble(assembler))
            targets.reverse.foreach(_.assemble(assembler))
        }
        else
        {
            val label = new Label
            exprs.head.assemble(assembler)
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__len__", "()J", false)
            assembler.visitor.visitLdcInsn(targets.length)
            assembler.visitor.visitInsn(Opcodes.I2L)
            assembler.visitor.visitInsn(Opcodes.LCMP)
            assembler.visitor.visitJumpInsn(Opcodes.IFEQ, label)
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(s"Needs ${targets.length} items to unpack")
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
            assembler.visitor.visitInsn(Opcodes.ATHROW)
            assembler.visitor.visitLabel(label)

            targets.zipWithIndex foreach {
                case (target, index) =>
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedInt")
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(index)
                    assembler.visitor.visitInsn(Opcodes.I2L)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedInt", "<init>", "(J)V", false)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__getitem__", "(Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                    target.assemble(assembler)
            }

            assembler.visitor.visitInsn(Opcodes.POP)
        }
    }
}
