package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler
import redscript.lang.SemanticError

class NodeReturn(exprs: List[NodeExpr], isTuple: Boolean) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        if (assembler.classes.top.method.isRoot)
            throw new SemanticError("`return` outside of functions")

        if (!isTuple)
        {
            exprs.head.assemble(assembler)
            assembler.visitor.visitInsn(Opcodes.ARETURN)
        }
        else
        {
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedTuple")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(exprs.length)
            assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")

            exprs.zipWithIndex foreach {
                case (expr, index) =>
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(index)
                    expr.assemble(assembler)
                    assembler.visitor.visitInsn(Opcodes.AASTORE)
            }

            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
            assembler.visitor.visitInsn(Opcodes.ARETURN)
        }
    }
}
