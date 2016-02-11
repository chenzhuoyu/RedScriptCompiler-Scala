package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeAssign(targets: List[NodeLValue], expr: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        expr.assemble(assembler)
        targets drop 1 foreach { _ => assembler.visitor.visitInsn(Opcodes.DUP) }
        targets.reverse foreach (_.assemble(assembler))
    }
}
