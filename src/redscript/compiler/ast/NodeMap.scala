package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeRow(val key: NodeExpr, val value: NodeExpr)
class NodeMap(rows: List[NodeRow]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedMap")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(rows.length)
        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "scala/Tuple2")

        rows.zipWithIndex foreach {
            case (row, index) =>
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(index)
                row.key.assemble(assembler)
                row.value.assemble(assembler)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "scala/Tuple2", "apply", "(Ljava/lang/Object;Ljava/lang/Object;)Lscala/Tuple2;", false)
                assembler.visitor.visitInsn(Opcodes.AASTORE)
        }

        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedMap", "<init>", "([Lscala/Tuple2;)V", false)
    }
}
