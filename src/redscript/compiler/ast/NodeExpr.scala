package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeExpr(left: Either[NodeExpr, NodeValue], op: String, right: NodeExpr) extends Node
{
    private val unaryOps = Map(
        "+" -> "__pos__",
        "-" -> "__neg__",
        "~" -> "__not__",
        "!" -> "__bool_not__"
    )

    private val binaryOps = Map(
        "+"  -> "__add__",
        "-"  -> "__sub__",
        "*"  -> "__mul__",
        "/"  -> "__div__",
        "%"  -> "__mod__",
        "**" -> "__pow__",
        "|"  -> "__or__",
        "&"  -> "__and__",
        "^"  -> "__xor__",
        "<<" -> "__lshift__",
        ">>" -> "__rshift__",
        "==" -> "__eq__",
        "<"  -> "__le__",
        ">"  -> "__ge__",
        "!=" -> "__neq__",
        "<=" -> "__leq__",
        ">=" -> "__geq__"
    )

    override def assemble(assembler: Assembler): Unit = left match
    {
        /* Unary expression */
        case null =>
            right.assemble(assembler)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", unaryOps(op), "()Lredscript/lang/RedObject;", false)

        /* Binary expression */
        case Left(x) => if (op != "in")
        {
            x.assemble(assembler)
            right.assemble(assembler)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", binaryOps(op), "(Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
        }
        else
        {
            x.assemble(assembler)
            right.assemble(assembler)
            assembler.visitor.visitInsn(Opcodes.SWAP)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__contains__", "(Lredscript/lang/RedObject;)Z", false)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedBoolean", "apply", "(Z)Lredscript/lang/RedBoolean;", false)
        }

        /* Simple value */
        case Right(x) => x.assemble(assembler)
    }
}
