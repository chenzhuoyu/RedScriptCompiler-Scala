package redscript.compiler

import org.objectweb.asm._
import org.objectweb.asm.commons.LocalVariablesSorter
import redscript.compiler.ast.{Identifier, Node}
import redscript.lang.{RedObject, SemanticError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Assembler private(callback: (String, Array[Byte]) => Unit)
{
    class Class(val name: String, val owner: Assembler, val writer: ClassWriter, val interfaces: Array[java.lang.Class[_]])
    {
        val fields = mutable.Map[String, Boolean]()
        val imports = mutable.Map[String, String]()
        val methods = mutable.Map[String, Method]()
        private val frees = ArrayBuffer[Identifier]()
        private val current = mutable.Stack[Method]()

        def freeVariables: Array[Identifier] = frees.toArray
        def markFreeVariable(varName: Identifier): Boolean = frees.contains(varName) || { frees.append(varName); true }

        def method: Method = current.top
        def endMethod: Method = current.pop
        def beginMethod(name: String, desc: String, isStatic: Boolean) = methods.get(name) match
        {
            case Some(_) => throw new SemanticError(s"Duplicate method name `$name`")
            case None    =>
                val mv = writer.visitMethod(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), name, desc, desc, null)
                val method = new Method(name, desc, isStatic, this, new LocalVariablesSorter(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), desc, mv))

                methods(name) = method
                current.push(method)
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

        def assemble(nodes: List[Node]): Unit =
        {
            beginMethod("<clinit>", "()V", isStatic = true)
            method.assemble(nodes)
            endMethod
            callback(name, writer.toByteArray)
        }
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

            /* stack informations */
            visitor.visitMaxs(0, 0)
            visitor.visitEnd()
        }
    }

    val classes = mutable.Stack[Class]()

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

    def endClass = classes.pop
    def beginClass(className: String, superClassName: String, interfaces: Array[String]) =
    {
        val intfs = interfaces map Class.forName
        val newClass = new Class(className, this, new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES), intfs)

        newClass.writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, className, null, superClassName, interfaces)
        classes.push(newClass)

        /* default constructor of this class */
        val default = newClass.writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)

        /* forward arguments to `__init__` */
        default.visitCode()
        default.visitVarInsn(Opcodes.ALOAD, 0)
        default.visitInsn(Opcodes.ICONST_0)
        default.visitTypeInsn(Opcodes.ANEWARRAY, "redscript/lang/RedObject")
        default.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "([Lredscript/lang/RedObject;)V", false)
        default.visitInsn(Opcodes.RETURN)
        default.visitMaxs(0, 0)
        default.visitEnd()

        /* constructor of the main class */
        val constructor = newClass.writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "([Lredscript/lang/RedObject;)V", null, null)

        /* forward arguments to `__init__` */
        constructor.visitCode()
        constructor.visitVarInsn(Opcodes.ALOAD, 0)
        constructor.visitMethodInsn(Opcodes.INVOKESPECIAL, superClassName, "<init>", "()V", false)
        constructor.visitVarInsn(Opcodes.ALOAD, 0)
        constructor.visitVarInsn(Opcodes.ALOAD, 1)
        constructor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, className, "__init__", "([Lredscript/lang/RedObject;)V", false)
        constructor.visitInsn(Opcodes.RETURN)
        constructor.visitMaxs(0, 0)
        constructor.visitEnd()
        newClass
    }

    def endInnerClass(superClassName: String) =
    {
        /* constructor of the main class */
        val cls = endClass
        val owner = classes.top.name
        val constructor = cls.writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", s"(L$owner;${"Lredscript/lang/RedObject;" * cls.freeVariables.length})V", null, null)

        /* field for owner instance */
        cls.fields("$owner") = false
        cls.writer.visitField(Opcodes.ACC_FINAL | Opcodes.ACC_SYNTHETIC, "$owner", s"L$owner;", null, null).visitEnd()

        /* store owner instance */
        constructor.visitCode()
        constructor.visitVarInsn(Opcodes.ALOAD, 0)
        constructor.visitVarInsn(Opcodes.ALOAD, 1)
        constructor.visitFieldInsn(Opcodes.PUTFIELD, cls.name, "$owner", s"L$owner;")

        /* free variables passed as closure */
        cls.freeVariables.zipWithIndex foreach {
            case (fv, index) =>
                constructor.visitVarInsn(Opcodes.ALOAD, 0)
                constructor.visitVarInsn(Opcodes.ALOAD, index + 2)
                constructor.visitFieldInsn(Opcodes.PUTFIELD, cls.name, s"freevar$$${fv.value}", "Lredscript/lang/RedObject;")
        }

        /* super constructor */
        constructor.visitVarInsn(Opcodes.ALOAD, 0)
        constructor.visitMethodInsn(Opcodes.INVOKESPECIAL, superClassName, "<init>", "()V", false)
        constructor.visitInsn(Opcodes.RETURN)
        constructor.visitMaxs(0, 0)
        constructor.visitEnd()
        cls.assemble(List())
    }

    def beginInnerClass(className: String, superClassName: String, interfaces: Array[String], isInlineClass: Boolean) =
    {
        val owner = classes.top.name
        val intfs = interfaces map Class.forName
        val access = if (isInlineClass) Opcodes.ACC_SUPER else Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER

        val fullName = s"$owner$$$className"
        val newClass = new Class(fullName, this, new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES), intfs)

        if (!isInlineClass)
        {
            newClass.writer.visitOuterClass(owner, null, null)
            classes.top.writer.visitInnerClass(fullName, owner, className, access)
        }
        else
        {
            newClass.writer.visitOuterClass(owner, classes.top.method.name, classes.top.method.desc)
            classes.top.writer.visitInnerClass(fullName, null, null, access)
        }

        newClass.writer.visit(Opcodes.V1_8, access, fullName, null, superClassName, interfaces)
        classes.push(newClass)
        newClass
    }
}

object Assembler
{
    case object GetStatic
    case object GetVirtual
    case object NoSuchField

    def assemble(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit =
    {
        val asm = new Assembler(callback)
        val cls = asm.beginClass(mainClass, "redscript/lang/RedObject", Array())

        cls.assemble(nodes)
        asm.endClass
    }

    def injectClass(name: String, bytecodes: Array[Byte]): Class[_] =
    {
        val loader = getClass.getClassLoader
        val injector = classOf[ClassLoader].getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

        injector.setAccessible(true)
        injector.invoke(loader, name, bytecodes, 0: Integer, bytecodes.length: Integer).asInstanceOf[Class[_]]
    }
}
