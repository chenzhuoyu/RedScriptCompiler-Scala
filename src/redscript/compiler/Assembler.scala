package redscript.compiler

import org.objectweb.asm._
import org.objectweb.asm.commons.LocalVariablesSorter
import redscript.compiler.ast.Node
import redscript.lang.{SemanticError, RedObject}

import scala.collection.mutable

class Assembler private(callback: (String, Array[Byte]) => Unit)
{
    class Class(val name: String, val owner: Assembler, val writer: ClassWriter)
    {
        val fields = mutable.Map[String, Boolean]()
        val imports = mutable.Map[String, String]()
        val methods = mutable.Map[String, Method]()
        private val current = mutable.Stack[Method]()

        def method: Method = current.top
        def endMethod: Method = current.pop
        def beginMethod(name: String, desc: String, isStatic: Boolean) = methods.get(name) match
        {
            case Some(_) => throw new SemanticError(s"Duplicate method name `$name`")
            case None    =>
                val mv = writer.visitMethod(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), name, desc, desc, null)
                val method = new Method(name, isStatic, this, new LocalVariablesSorter(Opcodes.ACC_PUBLIC | (if (isStatic) Opcodes.ACC_STATIC else 0), desc, mv))

                methods(name) = method
                current.push(method)
        }

        def getField(field: String) = fields.get(field) match
        {
            case Some(v) if  v => Assembler.GetStatic
            case Some(v) if !v => Assembler.GetVirtual
            case None          => Assembler.NoSuchField
        }

        def makeField(field: String, forceStatic: Boolean) = if (!fields.contains(field))
        {
            fields(field) = forceStatic | method.isStatic
            writer.visitField(if (forceStatic) Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC else Opcodes.ACC_PUBLIC, field, "Lredscript/lang/RedObject;", null, null).visitEnd()
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

    class Method(val name: String, val isStatic: Boolean, val owner: Class, val visitor: LocalVariablesSorter)
    {
        val locals = mutable.Map[String, Int]()
        val breaks = mutable.Stack[mutable.ArrayBuffer[Label]]()
        val continues = mutable.Stack[mutable.ArrayBuffer[Label]]()

        def getLocal(local: String) = locals.get(local)
        def makeLocal(local: String) = locals.getOrElseUpdate(local, visitor.newLocal(Type.getType(classOf[RedObject])))

        def markBreak(): Unit = breaks.isEmpty match
        {
            case true => throw new SemanticError("`break` outside loops")
            case false =>
                val label = new Label
                breaks.top += label
                visitor.visitJumpInsn(Opcodes.GOTO, label)
        }

        def markContinue(): Unit = continues.isEmpty match
        {
            case true => throw new SemanticError("`continue` outside loops")
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

            /* must return at the end of method */
            visitor.visitInsn(Opcodes.RETURN)
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

    def addClass(className: String) =
    {
        val name = className.replace('.', '/')
        val newClass = new Class(name, this, new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES))

        newClass.writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "redscript/lang/RedObject", null)
        classes.push(newClass)

        /* constructor of the main class */
        val ctor = newClass.writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "([Lredscript/lang/RedObject;)V", null, null)

        /* forward arguments to `__init__` */
        ctor.visitCode()
        ctor.visitVarInsn(Opcodes.ALOAD, 0)
        ctor.visitInsn(Opcodes.DUP)
        ctor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "<init>", "()V", false)
        ctor.visitVarInsn(Opcodes.ALOAD, 1)
        ctor.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "redscript/lang/RedObject", "__init__", "([Lredscript/lang/RedObject;)V", false)
        ctor.visitInsn(Opcodes.RETURN)
        ctor.visitMaxs(0, 0)
        ctor.visitEnd()
        newClass
    }
}

object Assembler
{
    case object GetStatic
    case object GetVirtual
    case object NoSuchField

    def assemble(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit = new Assembler(callback).addClass(mainClass).assemble(nodes)
    def injectClass(name: String, bytecodes: Array[Byte]): Class[_] =
    {
        val loader = getClass.getClassLoader
        val injector = classOf[ClassLoader].getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

        injector.setAccessible(true)
        injector.invoke(loader, name, bytecodes, 0: Integer, bytecodes.length: Integer).asInstanceOf[Class[_]]
    }
}
