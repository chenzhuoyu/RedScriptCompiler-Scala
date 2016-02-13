package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.Assembler

class NodeCase(val exprs: List[NodeExpr], val body: List[NodeStatement])
class NodeSelect(expr: NodeExpr, cases: List[NodeCase], default: Option[List[NodeStatement]]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val end = new Label
        val codes = new Array[Label](cases.length)

        expr.assemble(assembler)
        cases.zipWithIndex foreach {
            case (cond, index) =>
                val code = new Label
                codes(index) = code
                cond.exprs foreach { expr =>
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    expr.assemble(assembler)
                    assembler.visitor.visitInsn(Opcodes.SWAP)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__contains__", "(Lredscript/lang/RedObject;)Z", false)
                    assembler.visitor.visitJumpInsn(Opcodes.IFNE, code)
                }
        }

        default match
        {
            case None =>
            case Some(v) => v.foreach(_.assemble(assembler))
        }

        assembler.visitor.visitJumpInsn(Opcodes.GOTO, end)
        cases.zipWithIndex foreach {
            case (cond, index) =>
                assembler.visitor.visitLabel(codes(index))
                cond.body.foreach(_.assemble(assembler))
                assembler.visitor.visitJumpInsn(Opcodes.GOTO, end)
        }

        assembler.visitor.visitLabel(end)
        assembler.visitor.visitInsn(Opcodes.POP)
    }
}
