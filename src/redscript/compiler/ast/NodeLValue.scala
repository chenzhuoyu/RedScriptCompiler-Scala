package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeLValue(val value: NodeValue) extends Node
{
    override def assemble(assembler: Assembler): Unit = value.value match
    {
        case name: NodeName if value.modifiers.isEmpty =>
            val local = assembler.makeLocal(name.name.value)
            assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)

        case other =>
            other.assemble(assembler)
            value.modifiers dropRight 1 foreach {
                case Left(x)         => x.assemble(assembler)
                case Right(Left(x))  => x.assemble(assembler)
                case Right(Right(x)) => x.assemble(assembler)
            }

            assembler.visitor.visitInsn(Opcodes.SWAP)
            value.modifiers.last match
            {
                case Left(v)         => assembler.visitor.visitLdcInsn(v.name.value)
                case Right(Left(v))  => v.expr.assemble(assembler)
                case Right(Right(_)) =>
            }

            assembler.visitor.visitInsn(Opcodes.SWAP)
            value.modifiers.last match
            {
                case Left(v)         => assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__setattr__", "(Ljava/lang/String;Lredscript/lang/RedObject;)V", false)
                case Right(Left(v))  => assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__setitem__", "(Lredscript/lang/RedObject;Lredscript/lang/RedObject;)V", false)
                case Right(Right(_)) => throw new InternalError("Assign to invoke result")
            }
    }
}
