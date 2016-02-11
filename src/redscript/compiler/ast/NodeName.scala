package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.{Assembler, SyntaxError}
import redscript.lang.Builtins

class NodeName(val name: Identifier) extends Node
{
    override def assemble(assembler: Assembler): Unit = name.value match
    {
        case "null"  => assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedNull", "Null", "()Lredscript/lang/RedNull;", false)
        case "true"  => assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedBoolean", "True", "()Lredscript/lang/RedBoolean;", false)
        case "false" => assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedBoolean", "False", "()Lredscript/lang/RedBoolean;", false)

        case ident   => assembler.getLocal(ident) match
        {
            case Some(x) => assembler.visitor.visitVarInsn(Opcodes.ALOAD, x)
            case None    => assembler.hasField(ident) match
            {
                case true  =>
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                    assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, ident, "Lredscript/lang/RedObject;")

                case false => Builtins.builtins.get(ident) match
                {
                    case Some(x) => assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/Builtins", x, "()Lredscript/lang/RedObject;", false)
                    case None    => throw new SyntaxError(s"Identifier '$ident' not found", name.pos.line, name.pos.column)
                }
            }
        }
    }
}
