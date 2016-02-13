package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeArgument(val name: Identifier, val variant: Boolean)
class NodeFunction(name: Identifier, args: Option[List[NodeArgument]], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val owner = assembler.name
        val fname = s"$$FUNC_${name.value}"
        val className = s"$owner$$$fname"
        val ownerClass = assembler.classes.top
        val methodClass = assembler.beginClass(className, "redscript/lang/RedFunction")

        ownerClass.writer.visitInnerClass(className, owner, fname, Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC)
        methodClass.writer.visitOuterClass(owner, null, null)
        methodClass.beginMethod("__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", isStatic = false)
        methodClass.method.assemble(body)
        methodClass.endMethod
        methodClass.assemble(List())
        assembler.endClass
        assembler.visitor.visitTypeInsn(Opcodes.NEW, className)
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitInsn(Opcodes.ICONST_0)
        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "([Lredscript/lang/RedObject;)V", false)

        if (!assembler.classes.top.method.isRoot)
        {
            val local = assembler.makeLocal(name.value)
            assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
        }
        else
        {
            assembler.makeField(name.value, forceStatic = true)
            assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
        }
    }
}
