package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeIndex(val expr: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        expr.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__getitem__", "(Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
    }
}
