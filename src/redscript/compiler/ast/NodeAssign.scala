package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.{SyntaxError, Assembler}

class NodeAssign(targets: List[NodeLValue], expr: NodeExpr) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        targets foreach { target => target.value.value match
        {
            case v: NodeName if target.value.modifiers.isEmpty && v.name.value == "self" =>
                throw new SyntaxError("Re-assignment to `self`", v.name.pos.line, v.name.pos.column)

            case _ =>
        }}

        expr.assemble(assembler)
        targets drop 1 foreach { _ => assembler.visitor.visitInsn(Opcodes.DUP) }
        targets.reverse foreach (_.assemble(assembler))
    }
}
