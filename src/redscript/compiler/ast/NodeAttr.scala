package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeAttr(val name: Identifier) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitLdcInsn(name.value)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__getattr__", "(Ljava/lang/String;)Lredscript/lang/RedObject;", false)
    }
}
