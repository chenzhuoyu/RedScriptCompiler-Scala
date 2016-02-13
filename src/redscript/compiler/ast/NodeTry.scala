package redscript.compiler.ast

import org.objectweb.asm.{Opcodes, Label}
import redscript.compiler.Assembler

case class NodeExcept(except: NodeException, body: List[NodeStatement])
case class NodeException(names: List[List[Identifier]], name: Option[Identifier])

class NodeTry(body: List[NodeStatement], excepts: List[NodeExcept], cleanup: Option[List[NodeStatement]]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val end = new Label
        val exit = new Label
        val start = new Label

        assembler.visitor.visitLabel(start)
        body.foreach(_.assemble(assembler))
        assembler.visitor.visitJumpInsn(Opcodes.GOTO, exit)
        assembler.visitor.visitLabel(end)

        excepts foreach {
            case NodeExcept(null, statements) =>
                val handler = new Label
                assembler.visitor.visitLabel(handler)
                assembler.visitor.visitTryCatchBlock(start, end, handler, "java/lang/Throwable")
                assembler.visitor.visitInsn(Opcodes.POP)
                statements.foreach(_.assemble(assembler))
                assembler.visitor.visitJumpInsn(Opcodes.GOTO, exit)

            case NodeExcept(NodeException(types, name), statements) => types foreach { names =>
                val handler = new Label
                assembler.visitor.visitLabel(handler)
                assembler.visitor.visitTryCatchBlock(start, end, handler, names match
                {
                    case ident :: Nil => assembler.findImport(ident.value) match
                    {
                        case Some(v) => v
                        case None    => ident.value
                    }

                    case _ => names map (_.value) mkString "/"
                })

                name match
                {
                    case None =>
                        assembler.visitor.visitInsn(Opcodes.POP)

                    case Some(ident) =>
                        val local = assembler.makeLocal(ident.value)
                        assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedException", "apply", "(Ljava/lang/Throwable;)Lredscript/lang/RedException;", false)
                        assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
                }

                statements.foreach(_.assemble(assembler))
                assembler.visitor.visitJumpInsn(Opcodes.GOTO, exit)
            }
        }

        cleanup match
        {
            case None =>
                assembler.visitor.visitLabel(exit)

            case Some(v) =>
                val handler = new Label
                assembler.visitor.visitLabel(handler)
                assembler.visitor.visitTryCatchBlock(start, end, handler, null)
                assembler.visitor.visitVarInsn(Opcodes.ASTORE, 1)
                v.foreach(_.assemble(assembler))
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(exit)
                v.foreach(_.assemble(assembler))
        }
    }
}
