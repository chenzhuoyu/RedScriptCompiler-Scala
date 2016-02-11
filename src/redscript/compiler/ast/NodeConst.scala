package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeConst private(value: Either[Long, Either[Double, String]]) extends Node
{
    def this(value: Long)   = this(Left(value))
    def this(value: Double) = this(Right(Left(value)))
    def this(value: String) = this(Right(Right(value)))

    override def assemble(assembler: Assembler): Unit = value match
    {
        case Left(x) =>
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedInt")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedInt", "<init>", "(J)V", false)

        case Right(Left(x)) =>
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedFloat")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedFloat", "<init>", "(D)V", false)

        case Right(Right(x)) =>
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedString")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedString", "<init>", "(Ljava/lang/String;)V", false)
    }
}
