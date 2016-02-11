package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeDelete(value: NodeValue) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        value.value.assemble(assembler)
        value.modifiers dropRight 1 foreach {
            case Left(x)         => x.assemble(assembler)
            case Right(Left(x))  => x.assemble(assembler)
            case Right(Right(x)) => x.assemble(assembler)
        }

        value.modifiers.last match
        {
            case Left(v)         => assembler.visitor.visitLdcInsn(v.name.value)
            case Right(Left(v))  => v.expr.assemble(assembler)
            case Right(Right(_)) =>
        }

        value.modifiers.last match
        {
            case Left(v)         => assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__delattr__", "(Ljava/lang/String;)V", false)
            case Right(Left(v))  => assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__delitem__", "(Lredscript/lang/RedObject;)V", false)
            case Right(Right(_)) => throw new InternalError("Assign to invoke result")
        }
    }
}
