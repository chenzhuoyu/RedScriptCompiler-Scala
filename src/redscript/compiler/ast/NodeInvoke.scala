package redscript.compiler.ast

import org.objectweb.asm.Opcodes
import redscript.compiler.Assembler

class NodeParam(val expr: NodeExpr, val expand: Boolean)
class NodeInvoke(val args: List[NodeParam]) extends Node
{
    override def assemble(assembler: Assembler): Unit = args.find(_.expand) match
    {
        /* fixed parameters */
        case None =>
            assembler.visitor.visitLdcInsn(args.length)
            assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")

            args.zipWithIndex foreach {
                case (arg, index) =>
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(index)
                    arg.expr.assemble(assembler)
                    assembler.visitor.visitInsn(Opcodes.AASTORE)
            }

            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "redscript/lang/RedObject",
                "__invoke__",
                "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;",
                false)

        /* variable parameters */
        case Some(_) =>
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "scala/collection/mutable/ArrayBuffer")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(args.length)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "scala/collection/mutable/ArrayBuffer", "<init>", "(I)V", false)

            args foreach { arg => if (!arg.expand)
            {
                arg.expr.assemble(assembler)
                assembler.visitor.visitMethodInsn(
                    Opcodes.INVOKEVIRTUAL,
                    "scala/collection/mutable/ArrayBuffer",
                    "$plus$eq",
                    "(Ljava/lang/Object;)Lscala/collection/mutable/ArrayBuffer;",
                    false)
            }
            else
            {
                arg.expr.assemble(assembler)
                assembler.visitor.visitMethodInsn(
                    Opcodes.INVOKEVIRTUAL,
                    "scala/collection/mutable/ArrayBuffer",
                    "$plus$plus$eq",
                    "(Lscala/collection/TraversableOnce;)Lscala/collection/mutable/ArrayBuffer;",
                    false)
            }}

            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "scala/collection/mutable/ArrayBuffer", "length", "()I", false)
            assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
            assembler.visitor.visitInsn(Opcodes.DUP_X1)

            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKEINTERFACE,
                "scala/collection/TraversableOnce",
                "copyToArray",
                "(Ljava/lang/Object;)V",
                true
            )

            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKEVIRTUAL,
                "redscript/lang/RedObject",
                "__invoke__",
                "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;",
                false)
    }
}
