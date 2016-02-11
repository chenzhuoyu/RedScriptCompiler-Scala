package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeIncrement(target: NodeLValue, op: String, expr: NodeExpr) extends Node
{
    private val operators = Map(
        "+="  -> "__inc_add__",
        "-="  -> "__inc_sub__",
        "*="  -> "__inc_mul__",
        "/="  -> "__inc_div__",
        "%="  -> "__inc_mod__",
        "**=" -> "__inc_pow__",
        "|="  -> "__inc_or__",
        "&="  -> "__inc_and__",
        "^="  -> "__inc_xor__",
        "<<=" -> "__inc_lshift__",
        ">>=" -> "__inc_rshift__"
    )

    override def assemble(assembler: Assembler): Unit =
    {
        target.value.assemble(assembler)
        expr.assemble(assembler)
        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", operators(op), "(Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
        target.assemble(assembler)
    }
}
