package redscript.compiler.ast

import java.lang.reflect.{Method, Modifier}

import org.objectweb.asm.{Label, Opcodes, Type}
import redscript.compiler.Assembler
import redscript.lang._

import scala.language.postfixOps

class NodeClass(name: Identifier, parent: List[Identifier], intfs: List[List[Identifier]], body: List[NodeStatement]) extends Node
{
    private def setArgs(assembler: Assembler, argtype: Class[_], index: Int): Unit =
    {
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(index)

        argtype match
        {
            case java.lang.Byte.TYPE |
                 java.lang.Short.TYPE |
                 java.lang.Integer.TYPE =>
                assembler.visitor.visitVarInsn(Opcodes.ILOAD, index)
                assembler.visitor.visitInsn(Opcodes.I2L)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedInt", "apply", "(J)Lredscript/lang/RedInt;", false)

            case java.lang.Long.TYPE =>
                assembler.visitor.visitVarInsn(Opcodes.ILOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedInt", "apply", "(J)Lredscript/lang/RedInt;", false)

            case java.lang.Float.TYPE =>
                assembler.visitor.visitVarInsn(Opcodes.DLOAD, index)
                assembler.visitor.visitInsn(Opcodes.F2D)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedFloat", "apply", "(D)Lredscript/lang/RedFloat;", false)

            case java.lang.Double.TYPE =>
                assembler.visitor.visitVarInsn(Opcodes.DLOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedFloat", "apply", "(D)Lredscript/lang/RedFloat;", false)

            case java.lang.Boolean.TYPE =>
                assembler.visitor.visitVarInsn(Opcodes.ILOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedBoolean", "apply", "(Z)Lredscript/lang/RedBoolean;", false)

            case java.lang.Character.TYPE =>
                assembler.visitor.visitInsn(Opcodes.ICONST_1)
                assembler.visitor.visitIntInsn(Opcodes.NEWARRAY, Opcodes.T_CHAR)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                assembler.visitor.visitVarInsn(Opcodes.ILOAD, index)
                assembler.visitor.visitInsn(Opcodes.CASTORE)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/String", "valueOf", "([C)Ljava/lang/String;", false)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedString", "apply", "(Ljava/lang/String;)Lredscript/lang/RedString;", false)

            case v if v == classOf[Class[_]] =>
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedJavaClass")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedJavaClass", "<init>", "(Ljava/lang/Class;)V", false)

            case v if v == classOf[RedObject] =>
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, index)

            case v if v == classOf[java.lang.String] =>
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedString", "apply", "(Ljava/lang/String;)Lredscript/lang/RedString;", false)

            case _ =>
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedJavaObject")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, index)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedJavaObject", "<init>", "(Ljava/lang/Object;)V", false)
        }

        assembler.visitor.visitInsn(Opcodes.AASTORE)
    }

    private def makeReturn(assembler: Assembler, method: Method): Unit =
    {
        method.getReturnType match
        {
            case java.lang.Void.TYPE =>
                val voidCheck = new Label
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedNull", "Null", "()Lredscript/lang/RedNull;", false)
                assembler.visitor.visitJumpInsn(Opcodes.IF_ACMPEQ, voidCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `null` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(voidCheck)
                assembler.visitor.visitInsn(Opcodes.RETURN)

            case java.lang.Byte.TYPE |
                 java.lang.Short.TYPE |
                 java.lang.Integer.TYPE =>

                val typeCheck = new Label
                val valueCheckLow = new Label
                val valueCheckHigh = new Label

                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedInt")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `int` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedInt", "value", "()J", false)
                assembler.visitor.visitInsn(Opcodes.DUP)

                method.getReturnType match
                {
                    case java.lang.Byte.TYPE => assembler.visitor.visitLdcInsn(Byte.MinValue)
                    case java.lang.Short.TYPE => assembler.visitor.visitLdcInsn(Short.MinValue)
                    case java.lang.Integer.TYPE => assembler.visitor.visitLdcInsn(Int.MinValue)
                }

                assembler.visitor.visitInsn(Opcodes.LCMP)
                assembler.visitor.visitJumpInsn(Opcodes.IFGE, valueCheckLow)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return value is out of range (lower than minimum)")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(valueCheckLow)
                assembler.visitor.visitInsn(Opcodes.DUP)

                method.getReturnType match
                {
                    case java.lang.Byte.TYPE => assembler.visitor.visitLdcInsn(Byte.MaxValue)
                    case java.lang.Short.TYPE => assembler.visitor.visitLdcInsn(Short.MaxValue)
                    case java.lang.Integer.TYPE => assembler.visitor.visitLdcInsn(Int.MaxValue)
                }

                assembler.visitor.visitInsn(Opcodes.LCMP)
                assembler.visitor.visitJumpInsn(Opcodes.IFLE, valueCheckHigh)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return value is out of range (greater than maximum)")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(valueCheckHigh)
                assembler.visitor.visitInsn(Opcodes.IRETURN)

            case java.lang.Long.TYPE =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedInt")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `int` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedInt", "value", "()J", false)
                assembler.visitor.visitInsn(Opcodes.LRETURN)

            case java.lang.Float.TYPE =>
                val typeCheck = new Label
                val valueCheckLow = new Label
                val valueCheckHigh = new Label

                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedFloat")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `float` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedFloat", "value", "()D", false)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(Float.MinValue)
                assembler.visitor.visitInsn(Opcodes.LCMP)
                assembler.visitor.visitJumpInsn(Opcodes.IFGE, valueCheckLow)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return value is out of range (lower than minimum)")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(valueCheckLow)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn(Float.MaxValue)
                assembler.visitor.visitInsn(Opcodes.LCMP)
                assembler.visitor.visitJumpInsn(Opcodes.IFLE, valueCheckHigh)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return value is out of range (greater than maximum)")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(valueCheckHigh)
                assembler.visitor.visitInsn(Opcodes.FRETURN)

            case java.lang.Double.TYPE =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedFloat")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `float` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedFloat", "value", "()D", false)
                assembler.visitor.visitInsn(Opcodes.DRETURN)

            case java.lang.Boolean.TYPE =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedBoolean")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `bool` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedBoolean", "value", "()Z", false)
                assembler.visitor.visitInsn(Opcodes.IRETURN)

            case java.lang.Character.TYPE =>
                val typeCheck = new Label
                val lengthCheck = new Label

                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedString")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `str` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedString", "__len__", "()J", false)
                assembler.visitor.visitInsn(Opcodes.LCONST_1)
                assembler.visitor.visitInsn(Opcodes.LCMP)
                assembler.visitor.visitJumpInsn(Opcodes.IFEQ, lengthCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/ValueError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return value is out of range (lower than minimum)")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/ValueError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(lengthCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedString", "value", "()Ljava/lang/String;", false)
                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false)
                assembler.visitor.visitInsn(Opcodes.IRETURN)

            case v if v == classOf[Class[_]] =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedJavaClass")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `class` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedJavaClass", "cls", "()Ljava/lang/Class;", false)
                assembler.visitor.visitInsn(Opcodes.ARETURN)

            case v if v == classOf[RedObject] =>
                assembler.visitor.visitInsn(Opcodes.ARETURN)

            case v if v == classOf[java.lang.String] =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedString")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `str` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedString", "value", "()Ljava/lang/String;", false)
                assembler.visitor.visitInsn(Opcodes.ARETURN)

            case v =>
                val typeCheck = new Label
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitTypeInsn(Opcodes.INSTANCEOF, "redscript/lang/RedJavaObject")
                assembler.visitor.visitJumpInsn(Opcodes.IFNE, typeCheck)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/TypeError")
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitLdcInsn("Return type of `JavaObject` expected")
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/TypeError", "<init>", "(Ljava/lang/String;)V", false)
                assembler.visitor.visitInsn(Opcodes.ATHROW)
                assembler.visitor.visitLabel(typeCheck)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedJavaObject", "obj", "()Ljava/lang/Object;", false)
                assembler.visitor.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(v))
                assembler.visitor.visitInsn(Opcodes.ARETURN)
        }
    }

    override def assemble(assembler: Assembler): Unit =
    {
        val superClass = Class.forName(parent.length match
        {
            case 1 => assembler.findImport(parent.head.value) match
            {
                case None    => parent map (_.value) mkString "."
                case Some(v) => v
            }

            case _ => parent map (_.value) mkString "."
        })

        val newSuperClass    = if ( classOf[RedObject].isAssignableFrom(superClass)) Type.getInternalName(superClass) else "redscript/lang/RedObject"
        val bridgeSuperClass = if (!classOf[RedObject].isAssignableFrom(superClass)) Type.getInternalName(superClass) else "java/lang/Object"

        val fname = s"class$$${name.value}"
        val bname = s"${assembler.name}$$${name.value}"
        val newClass = assembler.makeClass(fname, newSuperClass, Array("redscript/lang/bridge/InstanceBridge"), hasOwnerClass = true) { newClass =>
        {
            newClass.makeMethod("<clinit>", "()V", isStatic = true)(newClass.method.assemble(body))
            newClass.makeMethod("<init>", "([Lredscript/lang/RedObject;)V", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "<init>", "()V", false)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, newClass.name, "__init__", "([Lredscript/lang/RedObject;)V", false)
                assembler.visitor.visitInsn(Opcodes.RETURN)
            }

            newClass.makeJavaField("$instance", s"L$bname;")
            newClass.makeMethod("toJavaInstance", "()Ljava/lang/Object;", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, newClass.name, "$instance", s"L$bname;")
                assembler.visitor.visitInsn(Opcodes.ARETURN)
            }

            classOf[RedObject].getMethods filter { method => method.getName.endsWith("__") && method.getName.startsWith("__") } foreach { method =>
            {
                val methodName = method.getName
                val methodDesc = Type.getMethodDescriptor(method)

                if (newClass.methods exists { case ((mname, desc), _) => desc != methodDesc && mname == methodName })
                {
                    newClass.makeMethod(method.getName, methodDesc, isStatic = false)
                    {
                        method.getName match
                        {
                            case "__init__" | "__invoke__" =>
                                assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, newClass.name, methodName, "Lredscript/lang/RedObject;")
                                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                                assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                                assembler.visitor.visitInsn(Opcodes.ICONST_1)
                                assembler.visitor.visitInsn(Opcodes.IADD)
                                assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                                assembler.visitor.visitInsn(Opcodes.DUP)
                                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                                assembler.visitor.visitInsn(Opcodes.AASTORE)
                                assembler.visitor.visitInsn(Opcodes.DUP_X2)
                                assembler.visitor.visitInsn(Opcodes.ICONST_1)
                                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                                assembler.visitor.visitInsn(Opcodes.ARRAYLENGTH)
                                assembler.visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/System", "arraycopy", "(Ljava/lang/Object;ILjava/lang/Object;II)V", false)
                                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                                assembler.visitor.visitInsn(Opcodes.POP)
                                assembler.visitor.visitInsn(Opcodes.RETURN)

                            case _ =>
                                assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, newClass.name, methodName, "Lredscript/lang/RedObject;")
                                assembler.visitor.visitLdcInsn(method.getParameterCount + 1)
                                assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                                assembler.visitor.visitInsn(Opcodes.DUP)
                                assembler.visitor.visitInsn(Opcodes.ICONST_0)
                                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                                assembler.visitor.visitInsn(Opcodes.AASTORE)

                                method.getParameterTypes.zipWithIndex foreach {
                                    case (argtype, index) =>
                                        setArgs(assembler, argtype, index + 1)
                                }

                                assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                                makeReturn(assembler, method)
                        }
                    }
                }
            }}
        }}

        val interfaces = (intfs map {
            case x if x.length >  1 => x map (_.value) mkString "/"
            case x if x.length == 1 => assembler findImport x.head.value match
            {
                case None    => x.head.value
                case Some(v) => v
            }
        } toArray) distinct

        assembler.makeClass(name.value, bridgeSuperClass, interfaces ++ ("redscript/lang/bridge/ClassBridge" :: Nil), hasOwnerClass = true) { bridgeClass =>
        {
            bridgeClass.makeMethod("<init>", s"(L${newClass.name};)V", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, bridgeSuperClass, "<init>", "()V", false)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitFieldInsn(Opcodes.PUTFIELD, newClass.name, "$instance", "Lredscript/lang/RedObject;")
                assembler.visitor.visitInsn(Opcodes.RETURN)
            }

            bridgeClass.makeSyntheticField("$instance")
            bridgeClass.makeMethod("<init>", "([Lredscript/lang/RedObject;)V", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitTypeInsn(Opcodes.NEW, newClass.name)
                assembler.visitor.visitInsn(Opcodes.DUP)
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, newClass.name, "<init>", "([Lredscript/lang/RedObject;)V", false)
                assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, bridgeClass.name, "<init>", s"(L${newClass.name};)V", false)
                assembler.visitor.visitInsn(Opcodes.RETURN)
            }

            bridgeClass.makeMethod("toRedObject", "()Lredscript/lang/RedObject;", isStatic = false)
            {
                assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, newClass.name, "$instance", "Lredscript/lang/RedObject;")
                assembler.visitor.visitInsn(Opcodes.ARETURN)
            }

            interfaces ++ (bridgeSuperClass :: Nil) foreach { name =>
            {
                Class.forName(name.replace("/", ".")).getMethods foreach {
                    case method if Modifier.isAbstract(method.getModifiers) =>
                        bridgeClass.makeMethod(method.getName, Type.getMethodDescriptor(method), isStatic = false)
                        {
                            assembler.visitor.visitFieldInsn(Opcodes.GETSTATIC, newClass.name, method.getName, "Lredscript/lang/RedObject;")
                            assembler.visitor.visitLdcInsn(method.getParameterCount + 1)
                            assembler.visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                            assembler.visitor.visitInsn(Opcodes.DUP)
                            assembler.visitor.visitInsn(Opcodes.ICONST_0)
                            assembler.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                            assembler.visitor.visitFieldInsn(Opcodes.GETFIELD, assembler.name, "$instance", "Lredscript/lang/RedObject;")
                            assembler.visitor.visitInsn(Opcodes.AASTORE)

                            method.getParameterTypes.zipWithIndex foreach {
                                case (argtype, index) =>
                                    setArgs(assembler, argtype, index + 1)
                            }

                            assembler.visitor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject;", "__invoke__", "([Lredscript/lang/RedObject;)Lredscript/lang/RedObject;", false)
                            makeReturn(assembler, method)
                        }

                    case _ =>
                }
            }}
        }}

        assembler.makeField(name.value, forceStatic = true)
        assembler.visitor.visitTypeInsn(Opcodes.NEW, "redscript/lang/RedClass")
        assembler.visitor.visitInsn(Opcodes.DUP)
        assembler.visitor.visitLdcInsn(name.value)
        assembler.visitor.visitLdcInsn(Type.getType(s"L${newClass.name};"))
        assembler.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedClass", "<init>", "(Ljava/lang/String;Ljava/lang/Class;)V", false)
        assembler.visitor.visitFieldInsn(Opcodes.PUTSTATIC, assembler.name, name.value, "Lredscript/lang/RedObject;")
    }
}
