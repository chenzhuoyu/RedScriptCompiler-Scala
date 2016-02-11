package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodePair(key: Identifier, value: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedTuple")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitInsn(Opcodes.ICONST_2)
        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitInsn(Opcodes.ICONST_0)
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedString")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(key.value)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedString", "<init>", "(Ljava/lang/String;)V", false)
        assembler.visitor.visitInsn(Opcodes.AASTORE)
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitInsn(Opcodes.ICONST_1)
        value.assemble(assembler)
        assembler.visitor.visitInsn(Opcodes.AASTORE)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
    }
}
