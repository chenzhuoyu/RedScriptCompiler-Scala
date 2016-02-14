package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.{SyntaxError, Assembler}

class NodeArgument(val name: Identifier, val variant: Boolean)
class NodeFunction(name: Identifier, args: List[NodeArgument], body: List[NodeStatement]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        val label = new Label
        val owner = assembler.name
        val isInline = !assembler.classes.top.method.isRoot
        val methodClass = assembler.beginInnerClass(s"$$FUNC_${name.value}", "redscript/lang/RedFunction", Array(), isInlineClass = isInline)

        methodClass.beginMethod("__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", isStatic = false)
        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
        assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)

        if (args.isEmpty)
        {
            assembler.visitor.visitJumpInsn(Opcodes.IFEQ, label)
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(s"Function `${name.value}` takes no arguments")
        }
        else if (!args.last.variant)
        {
            assembler.visitor.visitLdcInsn(args.length)
            assembler.visitor.visitJumpInsn(Opcodes.IF_ICMPEQ, label)
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(s"Function `${name.value}` takes exactly ${args.length} argument(s)")
        }
        else
        {
            assembler.visitor.visitLdcInsn(args.length - 1)
            assembler.visitor.visitJumpInsn(Opcodes.IF_ICMPGE, label)
            assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitLdcInsn(s"Function `${name.value}` takes at least ${args.length - 1} argument(s)")
        }

        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ArgumentError", "<init>", "(Ljava/lang/String;)V", false)
        assembler.visitor.visitInsn(Opcodes.ATHROW)
        assembler.visitor.visitLabel(label)

        if (args.nonEmpty)
        {
            if (!args.last.variant)
            {
                args.zipWithIndex foreach {
                    case (arg, index) =>
                        val local = assembler.makeLocal(arg.name.value)
                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                        assembler.visitor.visitLdcInsn(index)
                        assembler.visitor.visitInsn(Opcodes.AALOAD)
                        assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
                }
            }
            else
            {
                (args dropRight 1).zipWithIndex foreach {
                    case (arg, index) =>
                        val local = assembler.makeLocal(arg.name.value)
                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                        assembler.visitor.visitLdcInsn(index)
                        assembler.visitor.visitInsn(Opcodes.AALOAD)
                        assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
                }

                val local = assembler.makeLocal(args.last.name.value)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedTuple")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitLdcInsn(args.length - 1)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                assembler.visitor.visitLdcInsn(args.length - 1)
                assembler.visitor.visitInsn(Opcodes.ISUB)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                assembler.visitor.visitInsn(Opcodes.SWAP)
                assembler.visitor.visitInsn(Opcodes.DUP2_X2)
                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                assembler.visitor.visitInsn(Opcodes.SWAP)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
                assembler.visitor.visitInsn(Opcodes.POP)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
                assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
            }
        }

        methodClass.method.assemble(body)
        methodClass.endMethod
        assembler.endInnerClass("redscript/lang/RedFunction")
        assembler.visitor.visitTypeInsn(Opcodes.NEW, methodClass.name)
        assembler.visitor.visitInsn(Opcodes.DUP)

        if (isInline)
        {
            val local = assembler.makeLocal(name.value)
            assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)

            methodClass.freeVariables foreach { fv => assembler.getLocal(fv.value) match
            {
                case Some(v) => assembler.visitor.visitVarInsn(Opcodes.ALOAD, v)
                case None    => assembler.getField(fv.value) match
                {
                    case Assembler.GetStatic =>
                        assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, fv.value, "Lredscript/lang/RedObject;")

                    case Assembler.GetVirtual =>
                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                        assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, owner, fv.value, "Lredscript/lang/RedObject;")

                    case Assembler.NoSuchField =>
                        if (assembler.markFreeVariable(fv))
                            assembler.classes.top.makeSyntheticField(s"$$FV_${fv.value}")

                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                        assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, s"$$FV_${fv.value}", "Lredscript/lang/RedObject;")
                }
            }}

            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, methodClass.name, "<init>", s"(L$owner;${"Lredscript/lang/RedObject;" * methodClass.freeVariables.length})V", false)
            assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
        }
        else
        {
            assembler.makeField(name.value, forceStatic = true)
            assembler.visitor.visitInsn(Opcodes.ACONST_NULL)

            methodClass.freeVariables foreach { fv => assembler.getLocal(fv.value) match
            {
                case Some(v) => assembler.visitor.visitVarInsn(Opcodes.ALOAD, v)
                case None    => assembler.getField(fv.value) match
                {
                    case Assembler.GetStatic =>
                        assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, fv.value, "Lredscript/lang/RedObject;")

                    case Assembler.GetVirtual =>
                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                        assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, owner, fv.value, "Lredscript/lang/RedObject;")

                    case Assembler.NoSuchField =>
                        throw new SyntaxError(s"Identifier '${fv.value}' not found", name.pos.line, name.pos.column)
                }
            }}

            assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, methodClass.name, "<init>", s"(L$owner;${"Lredscript/lang/RedObject;" * methodClass.freeVariables.length})V", false)
            assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
        }

//        assembler.classes.top.beginMethod(name.value, s"(${"Lredscript/lang/RedObject;" * args.length})Lredscript/lang/RedObject;", isStatic = false)
//        assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
//        assembler.visitor.visitLdcInsn(args.length)
//        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
//        assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
//        assembler.visitor.visitInsn(Opcodes.ARETURN)
//        assembler.visitor.visitMaxs(0, 0)
//        assembler.visitor.visitEnd()
//        assembler.classes.top.endMethod
    }
}
