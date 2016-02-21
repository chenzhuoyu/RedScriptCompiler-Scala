package redscript.compiler.ast

import org.objectweb.asm.{Label, Opcodes}
import redscript.compiler.{Assembler, SyntaxError}

private object LambdaCounter
{
    private var index: Int = 0
    def apply(): Int =
    {
        index += 1
        index
    }
}

class NodeArgument(val name: Identifier, val variant: Boolean)
class NodeFunction(name: Identifier, args: List[NodeArgument], body: List[NodeStatement], decorators: List[NodeExpr]) extends Node
{
    override def assemble(assembler: Assembler): Unit =
    {
        decorators foreach { decorator =>
            decorator.assemble(assembler)
            assembler.visitor.visitInsn(Opcodes.ICONST_1)
            assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
            assembler.visitor.visitInsn(Opcodes.DUP)
            assembler.visitor.visitInsn(Opcodes.ICONST_0)
        }

        val label = new Label
        val owner = assembler.name
        val isInline = !assembler.classes.top.method.isRoot
        val funcName = if (name != null) name.value else s"lambda$$${LambdaCounter()}"
        val methodClass = assembler.makeInnerClass(s"func$$$funcName", "redscript/lang/RedFunction", Array()) { methodClass =>
        {
            methodClass.makeMethod("__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)

                if (args.isEmpty)
                {
                    assembler.visitor.visitJumpInsn(Opcodes.IFEQ, label)
                    assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(s"Function `$funcName` takes no arguments")
                }
                else if (!args.last.variant)
                {
                    assembler.visitor.visitLdcInsn(args.length)
                    assembler.visitor.visitJumpInsn(Opcodes.IF_ICMPEQ, label)
                    assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(s"Function `$funcName` takes exactly ${args.length} argument(s)")
                }
                else
                {
                    assembler.visitor.visitLdcInsn(args.length - 1)
                    assembler.visitor.visitJumpInsn(Opcodes.IF_ICMPGE, label)
                    assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ArgumentError")
                    assembler.visitor.visitInsn(Opcodes.DUP)
                    assembler.visitor.visitLdcInsn(s"Function `$funcName` takes at least ${args.length - 1} argument(s)")
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
                    else if (args.length == 1)
                    {
                        val local = assembler.makeLocal(args.head.name.value)
                        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedTuple")
                        assembler.visitor.visitInsn(Opcodes.DUP)
                        assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
                        assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
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
                        assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                        assembler.visitor.visitInsn(Opcodes.DUP_X2)
                        assembler.visitor.visitInsn(Opcodes.DUP)
                        assembler.visitor.visitInsn(Opcodes.ICONST_0)
                        assembler.visitor.visitInsn(Opcodes.SWAP)
                        assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                        assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
                        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedTuple", "<init>", "([Lredscript/lang/RedObject;)V", false)
                        assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
                    }
                }

                methodClass.method.assemble(body)
            }
        }}

        assembler.visitor.visitTypeInsn(Opcodes.NEW, methodClass.name)
        assembler.visitor.visitInsn(Opcodes.DUP)

        if (isInline)
        {
            assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
            methodClass.freeVariables foreach { fv => assembler.getLocal(fv.value) match
            {
                case Some(v) => assembler.visitor.visitVarInsn(Opcodes.ALOAD, v)
                case None    =>
                    if (assembler.markFreeVariable(fv))
                        assembler.classes.top.makeSyntheticField(s"freevar$$${fv.value}")

                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                    assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, s"freevar$$${fv.value}", "Lredscript/lang/RedObject;")
            }}

            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKESPECIAL,
                methodClass.name,
                "<init>",
                s"(L$owner;${"Lredscript/lang/RedObject;" * methodClass.freeVariables.length})V",
                false)

            if (name != null)
            {
                val local = assembler.makeLocal(name.value)
                assembler.visitor.visitVarInsn(Opcodes.ASTORE, local)
            }
        }
        else
        {
            assembler.visitor.visitInsn(Opcodes.ACONST_NULL)
            methodClass.freeVariables foreach { fv => assembler.getLocal(fv.value) match
            {
                case Some(v) => assembler.visitor.visitVarInsn(Opcodes.ALOAD, v)
                case None    => throw new SyntaxError(s"Identifier '${fv.value}' not found", name.pos.line, name.pos.column)
            }}

            assembler.visitor.visitMethodInsn(
                Opcodes.INVOKESPECIAL,
                methodClass.name,
                "<init>",
                s"(L$owner;${"Lredscript/lang/RedObject;" * methodClass.freeVariables.length})V",
                false)

            decorators foreach { _ =>
                assembler.visitor.visitInsn(Opcodes.AASTORE)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
            }

            if (name != null)
            {
                assembler.makeField(name.value, forceStatic = true)
                assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
            }
        }

        if (name != null)
        {
            if (args.isEmpty)
            {
                assembler.classes.top.makeMethod(name.value, "()Lredscript/lang/RedObject;", isStatic = !isInline)
                {
                    assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
                    assembler.visitor.visitInsn(Opcodes.ICONST_0)
                    assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                    assembler.visitor.visitInsn(Opcodes.ARETURN)
                }
            }
            else if (!args.last.variant)
            {
                assembler.classes.top.makeMethod(name.value, s"(${"Lredscript/lang/RedObject;" * args.length})Lredscript/lang/RedObject;", isStatic = !isInline)
                {
                    assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
                    assembler.visitor.visitLdcInsn(args.length)
                    assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")

                    args.zipWithIndex foreach {
                        case (arg, index) =>
                            assembler.visitor.visitInsn(Opcodes.DUP)
                            assembler.visitor.visitLdcInsn(index)
                            assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) index + 1 else index)
                            assembler.visitor.visitInsn(Opcodes.AASTORE)
                    }

                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                    assembler.visitor.visitInsn(Opcodes.ARETURN)
                }
            }
            else if (args.length == 1)
            {
                assembler.classes.top.makeMethod(name.value, s"([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", isStatic = !isInline)
                {
                    assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) 1 else 0)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                    assembler.visitor.visitInsn(Opcodes.ARETURN)
                }
            }
            else
            {
                assembler.classes.top.makeMethod(name.value, s"(${"Lredscript/lang/RedObject;" * (args.length - 1)}[Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", isStatic = !isInline)
                {
                    assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, owner, name.value, "Lredscript/lang/RedObject;")
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) args.length else args.length - 1)
                    assembler.visitor.visitInsn(Opcodes.ICONST_0)
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) args.length else args.length - 1)
                    assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                    assembler.visitor.visitLdcInsn(args.length - 1)
                    assembler.visitor.visitInsn(Opcodes.IADD)
                    assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")

                    args.zipWithIndex dropRight 1 foreach {
                        case (arg, index) =>
                            assembler.visitor.visitInsn(Opcodes.DUP)
                            assembler.visitor.visitLdcInsn(index)
                            assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) index + 1 else index)
                            assembler.visitor.visitInsn(Opcodes.AASTORE)
                    }

                    assembler.visitor.visitInsn(Opcodes.DUP_X2)
                    assembler.visitor.visitLdcInsn(args.length - 1)
                    assembler.visitor.visitVarInsn(Opcodes.ALOAD, if (isInline) args.length else args.length - 1)
                    assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
                    assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                    assembler.visitor.visitInsn(Opcodes.ARETURN)
                }
            }
        }
    }
}
