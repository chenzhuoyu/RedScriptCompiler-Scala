package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeArray(items: List[NodeExpr]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedArray")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "scala/collection/mutable/ArrayBuffer")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(items.length)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "scala/collection/mutable/ArrayBuffer", "<init>", "(I)V", false)

        items foreach { item =>
            item.assemble(assembler)
            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "scala/collection/mutable/ArrayBuffer",
                "$plus$eq",
                "(Ljava/lang/Object;)Lscala/collection/mutable/ArrayBuffer;",
                false)
        }

        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedArray", "<init>", "(Lscala/collection/mutable/ArrayBuffer;)V", false)
    }
}
