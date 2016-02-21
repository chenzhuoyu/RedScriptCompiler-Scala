package redscript.compiler

import org.objectweb.asm._
import org.objectweb.asm.commons.LocalVariablesSorter
import redscript.compiler.ast.{Identifier, Node}
import redscript.lang.{RedObject, SemanticError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Assembler private(callback: (String, Array[Byte]) => Unit)
{
    val classes = mutable.Stack[Assembler.Class]()

    def name: String = classes.top.name
    def writer: ClassWriter = classes.top.writer
    def visitor: LocalVariablesSorter = classes.top.method.visitor

    def getLocal(local: String) = classes.top.method.getLocal(local)
    def makeLocal(local: String) = classes.top.method.makeLocal(local)

    def getField(field: String) = classes.top.getField(field)
    def makeField(field: String, forceStatic: Boolean = false) = classes.top.makeField(field, forceStatic)

    def findImport(name: String) = classes.top.findImport(name)
    def cacheImport(name: String, className: String) = classes.top.cacheImport(name, className)

    def freeVariables = classes.top.freeVariables
    def markFreeVariable(varName: Identifier) = classes.top.markFreeVariable(varName)

    def makeClass(className: String, superClassName: String, interfaces: Array[String], hasOwnerClass: Boolean)(assembleBody: (Assembler.Class) => Unit) =
    {
        val fname = if (hasOwnerClass) s"$name$$$className" else className
        val newClass = new Assembler.Class(fname, superClassName, this, new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES))

        if (hasOwnerClass)
        {
            writer.visitInnerClass(fname, name, className, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER | Opcodes.ACC_STATIC)
            newClass.writer.visitOuterClass(name, null, null)
        }

        newClass.writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, fname, null, superClassName, interfaces)
        classes.push(newClass)

        /* inner classes doesn't need default constructor */
        if (!hasOwnerClass)
        {
            newClass.makeMethod("<init>", "()V", isStatic = false)
            {
                visitor.visitVarInsn(Opcodes.ALOAD, 0)
                visitor.visitInsn(Opcodes.ICONST_0)
                visitor.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
                visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, fname, "<init>", "([Lredscript/lang/RedObject;)V", false)
                visitor.visitInsn(Opcodes.RETURN)
            }
        }

        assembleBody(newClass)
        callback(newClass.name, newClass.writer.toByteArray)
        classes.pop
    }

    def makeInnerClass(className: String, superClassName: String, interfaces: Array[String])(assembleBody: (Assembler.Class) => Unit) =
    {
        val owner = name
        val fname = s"$owner$$$className"
        val newClass = new Assembler.Class(fname, superClassName, this, new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES))

        /* basic class informations */
        writer.visitInnerClass(fname, null, null, Opcodes.ACC_SUPER)
        newClass.writer.visit(Opcodes.V1_8, Opcodes.ACC_SUPER, fname, null, superClassName, interfaces)
        newClass.writer.visitOuterClass(owner, classes.top.method.name, classes.top.method.desc)

        /* assemble body */
        classes.push(newClass)
        assembleBody(newClass)

        /* constructor of the inner class */
        newClass.makeMethod("<init>", s"(L$owner;${"Lredscript/lang/RedObject;" * newClass.freeVariables.length})V", isStatic = false)
        {
            newClass.method.visitor.visitCode()
            newClass.method.visitor.visitVarInsn(Opcodes.ALOAD, 0)
            newClass.method.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, newClass.superClass, "<init>", "()V", false)

            /* free variables passed as closure */
            newClass.freeVariables.zipWithIndex foreach{
                case (fv, index) =>
                    visitor.visitVarInsn(Opcodes.ALOAD, 0)
                    visitor.visitVarInsn(Opcodes.ALOAD, index + 2)
                    visitor.visitFieldInsn(Opcodes.PUTFIELD, newClass.name, s"freevar$$${fv.value}", "Lredscript/lang/RedObject;")
            }

            /* method must have a RETURN */
            visitor.visitInsn(Opcodes.RETURN)
        }

        callback(newClass.name, newClass.writer.toByteArray)
        classes.pop
    }
}

object Assembler
{
    case object GetStatic
    case object GetVirtual
    case object NoSuchField

    class Class(val name: String, val superClass: String, val owner: Assembler, val writer: ClassWriter)
    {
        val fields = mutable.Map[String, Boolean]()
        val imports = mutable.Map[String, String]()
        val methods = mutable.Map[(String, String), Method]()
        private val frees = ArrayBuffer[Identifier]()
        private val current = mutable.Stack[Method]()

        def freeVariables: Array[Identifier] = frees.toArray
        def markFreeVariable(varName: Identifier): Boolean = frees.contains(varName) || { frees.append(varName); true }

        def method: Method = current.top
        def makeMethod(name: String, desc: String, isStatic: Boolean)(body: => Unit) =
        {
            methods.get((name, desc)) match
            {
                case Some(_) => throw new SemanticError(s"Duplicate method name `$name`")
                case None    =>
                    val mv = writer.visitMethod(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), name, desc, desc, null)
                    val method = new Method(name, desc, isStatic, this, new LocalVariablesSorter(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), desc, mv))

                    current.push(method)
                    methods((name, desc)) = method
            }

            body
            method.visitor.visitMaxs(0, 0)
            method.visitor.visitEnd()
            current.pop
        }

        def getField(field: String) = fields.get(field) match
        {
            case Some(v) if  v => Assembler.GetStatic
            case Some(v) if !v => Assembler.GetVirtual
            case None          => Assembler.NoSuchField
        }

        def makeField(field: String, isStatic: Boolean) = if (!fields.contains(field))
        {
            fields(field) = isStatic
            writer.visitField(if (isStatic) Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC else Opcodes.ACC_PUBLIC, field, "Lredscript/lang/RedObject;", null, null).visitEnd()
        }

        def makeSyntheticField(field: String) = if (!fields.contains(field))
        {
            fields(field) = false
            writer.visitField(Opcodes.ACC_FINAL | Opcodes.ACC_SYNTHETIC, field, "Lredscript/lang/RedObject;", null, null).visitEnd()
        }

        def findImport(name: String) = imports.get(name)
        def cacheImport(name: String, className: String) = imports(name) = className
    }

    class Method(val name: String, val desc: String, val isStatic: Boolean, val owner: Class, val visitor: LocalVariablesSorter)
    {
        val locals = mutable.Map[String, Int]()
        val breaks = mutable.Stack[mutable.ArrayBuffer[Label]]()
        val continues = mutable.Stack[mutable.ArrayBuffer[Label]]()

        def isRoot: Boolean = name == "<clinit>"
        def getLocal(local: String) = locals.get(local)
        def makeLocal(local: String) = locals.getOrElseUpdate(local, visitor.newLocal(Type.getType(classOf[RedObject])))

        def markBreak(): Unit = breaks.isEmpty match
        {
            case true => throw new SemanticError("`break` outside of loops")
            case false =>
                val label = new Label
                breaks.top += label
                visitor.visitJumpInsn(Opcodes.GOTO, label)
        }

        def markContinue(): Unit = continues.isEmpty match
        {
            case true => throw new SemanticError("`continue` outside of loops")
            case false =>
                val label = new Label
                continues.top += label
                visitor.visitJumpInsn(Opcodes.GOTO, label)
        }

        def enterLoop(): Unit =
        {
            breaks.push(mutable.ArrayBuffer[Label]())
            continues.push(mutable.ArrayBuffer[Label]())
        }

        def patchBreaks(): Unit = breaks.pop foreach visitor.visitLabel
        def patchContinues(): Unit = continues.pop foreach visitor.visitLabel

        def assemble(nodes: List[Node]): Unit =
        {
            /* local variable scopes */
            val end = new Label
            val start = new Label

            /* generates each nodes */
            visitor.visitLabel(start)
            nodes.foreach(_.assemble(owner.owner))
            visitor.visitLabel(end)

            /* declare local variables */
            locals foreach {
                case (local, index) =>
                    visitor.visitLocalVariable(local, "Lredscript/lang/RedObject;", null, start, end, index)
            }

            /* function must return */
            name match
            {
                case "<init>"   => visitor.visitInsn(Opcodes.RETURN)
                case "<clinit>" => visitor.visitInsn(Opcodes.RETURN)
                case _          =>
                    visitor.visitMethodInsn(Opcodes.INVOKESTATIC, "redscript/lang/RedNull", "Null", "()Lredscript/lang/RedNull;", false)
                    visitor.visitInsn(Opcodes.ARETURN)
            }
        }
    }

    def assemble(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit =
    {
        new Assembler(callback).makeClass(mainClass, "redscript/lang/RedObject", Array(), hasOwnerClass = false) { newClass =>
        {
            newClass.makeMethod("<clinit>", "()V", isStatic = true)(newClass.method.assemble(nodes))
            newClass.makeMethod("<init>", "([Lredscript/lang/RedObject;)V", isStatic = false)
            {
                newClass.method.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                newClass.method.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "<init>", "()V", false)
                newClass.method.visitor.visitVarInsn(Opcodes.ALOAD, 0)
                newClass.method.visitor.visitVarInsn(Opcodes.ALOAD, 1)
                newClass.method.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "__init__", "([Lredscript/lang/RedObject;)V", false)
                newClass.method.visitor.visitInsn(Opcodes.RETURN)
            }
        }}
    }

    def injectClass(name: String, bytecodes: Array[Byte]): java.lang.Class[_] =
    {
        val loader = getClass.getClassLoader
        val injector = classOf[ClassLoader].getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

        injector.setAccessible(true)
        injector.invoke(loader, name, bytecodes, 0: Integer, bytecodes.length: Integer).asInstanceOf[java.lang.Class[_]]
    }
}
