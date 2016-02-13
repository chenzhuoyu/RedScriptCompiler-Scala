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
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedInt", "apply", "(J)Lredscript/lang/RedInt;", false)

        case Right(Left(x)) =>
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedFloat", "apply", "(D)Lredscript/lang/RedFloat;", false)

        case Right(Right(x)) =>
            assembler.visitor.visitLdcInsn(x)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedString", "apply", "(Ljava/lang/String;)Lredscript/lang/RedString;", false)
    }
}
