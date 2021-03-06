package redscript.compiler.ast

import org.objectweb.asm.{Opcodes, Type}
import redscript.compiler.Assembler
import redscript.lang.{RedClass, SemanticError}

class NodeImport(names: List[Identifier], alias: Identifier) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val desc = names map (_.value) mkString "/"
        val name = if (alias == null) names.last.value else alias.value

        assembler.findImport(name) match
        {
            case None => if (!RedClass.isRedClass(desc))
            {
                assembler.makeField(name, forceStatic = true)
                assembler.cacheImport(name, desc)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedJavaClass")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(Type.getType(s"L$desc;"))
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedJavaClass", "<init>", "(Ljava/lang/Class;)V", false)
                assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, assembler.name, name, "Lredscript/lang/RedObject;")
            }
            else
            {
                assembler.makeField(name, forceStatic = true)
                assembler.cacheImport(name, desc)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedInternalClass")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(Type.getType(s"L$desc;"))
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedInternalClass", "<init>", "(Ljava/lang/Class;)V", false)
                assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, assembler.name, name, "Lredscript/lang/RedObject;")
            }

            case Some(_) =>
                throw new SemanticError(s"Import conflict for name `$name`, consider using alias")
        }
    }
}
