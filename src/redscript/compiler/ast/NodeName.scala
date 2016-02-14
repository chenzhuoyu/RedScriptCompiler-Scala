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
            case None    => assembler.getField(ident) match
            {
                case Assembler.GetStatic =>
                    assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, assembler.name, ident, "Lredscript/lang/RedObject;")

                case Assembler.GetVirtual =>
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                    assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, ident, "Lredscript/lang/RedObject;")

                case Assembler.NoSuchField => Builtins.builtins.get(ident) match
                {
                    case Some(x) => assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/Builtins", x, "()Lredscript/lang/RedObject;", false)
                    case None    =>
                        if (assembler.classes.length < 2)
                            throw new SyntaxError(s"Identifier '$ident' not found", name.pos.line, name.pos.column)

                        if (!(assembler.classes exists (_.getField(ident) == Assembler.GetStatic)))
                        {
                            if (assembler.markFreeVariable(name))
                                assembler.classes.top.makeSyntheticField(s"freevar$$$ident")

                            assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                            assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, s"freevar$$$ident", "Lredscript/lang/RedObject;")
                        }
                        else
                        {
                            var classDesc = ""
                            var className = ""
                            var description = "redscript/lang/RedObject"

                            assembler.classes.reverse foreach { nested =>
                                nested.getField(ident) match
                                {
                                    case Assembler.GetStatic   =>
                                        classDesc = description
                                        className = nested.name
                                        description = nested.name

                                    case Assembler.GetVirtual  => description = nested.name
                                    case Assembler.NoSuchField => description = nested.name
                                }
                            }

                            assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, className, ident, s"L$classDesc;")
                        }
                }
            }
        }
    }
}
