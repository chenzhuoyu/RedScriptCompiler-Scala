package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeImport(names: List[Identifier], alias: Identifier) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val name = if (alias == null) names.last.value else alias.value
        assembler.makeField(name, "Lredscript/lang/RedObject;")
        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedJavaClass")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(names map (_.value) mkString ".")
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Class", "forName", "(Ljava/lang/String;)Ljava/lang/Class;", false)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedJavaClass", "<init>", "(Ljava/lang/Class;)V", false)
        assembler.visitor.visitFieldInsn(Opcodes.PUTFIELD, assembler.name, name, "Lredscript/lang/RedObject;")
    }
}
