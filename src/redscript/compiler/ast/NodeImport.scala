package redscript.compiler.ast

import org.objectweb.asm.{Type, Opcodes}
import redscript.compiler.Assembler

class NodeImport(names: List[Identifier], alias: Identifier) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val desc = names map (_.value) mkString "/"
        val name = if (alias == null) names.last.value else alias.value

        assembler.makeField(name, forceStatic = true)
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedJavaClass")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(Type.getType(s"L$desc;"))
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedJavaClass", "<init>", "(Ljava/lang/Class;)V", false)
        assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, assembler.name, name, "Lredscript/lang/RedObject;")
    }
}
