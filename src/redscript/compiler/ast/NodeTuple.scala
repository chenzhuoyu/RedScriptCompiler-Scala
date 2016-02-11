package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeTuple(items: List[NodeExpr]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedTuple")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(items.length)
        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")

        items.zipWithIndex foreach {
            case (item, index) =>
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(index)
                item.assemble(assembler)
                assembler.visitor.visitInsn(Opcodes.AASTORE)
        }

        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
    }
}
