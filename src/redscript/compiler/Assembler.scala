package redscript.compiler

import org.objectweb.asm._
import org.objectweb.asm.commons.LocalVariablesSorter
import redscript.compiler.ast.Node
import redscript.lang.RedObject

import scala.collection.mutable

class Assembler private(mainClass: String, nodes: List[Node], val isStatic: Boolean, val callback: (String, Array[Byte]) => Unit)
{
    val name = mainClass.replace('.', '/')
    val locals = mutable.Map[String, Int]()
    val fields = mutable.Map[String, Boolean]()
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    val visitor = new LocalVariablesSorter(if (isStatic) Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC else Opcodes.ACC_PUBLIC, "([Ljava/lang/String;)V",
        writer.visitMethod(if (isStatic) Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC else Opcodes.ACC_PUBLIC, if (isStatic) "<clinit>" else "<init>", "()V", null, null))

    val breaks = mutable.Stack[mutable.ArrayBuffer[Label]]()
    val continues = mutable.Stack[mutable.ArrayBuffer[Label]]()

    def getLocal(local: String) = locals.get(local)
    def makeLocal(local: String) = locals.getOrElseUpdate(local, visitor.newLocal(Type.getType(classOf[RedObject])))

    def getField(field: String) = fields.get(field) match
    {
        case None    => Assembler.NoSuchField
        case Some(v) => if (v) Assembler.GetStatic else Assembler.GetVirtual
    }

    def makeField(field: String, forceStatic: Boolean = false) = if (!fields.contains(field))
    {
        fields(field) = isStatic | forceStatic
        writer.visitField(if (isStatic || forceStatic) Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC else Opcodes.ACC_PUBLIC, field, "Lredscript/lang/RedObject;", null, null).visitEnd()
    }

    def assemble(): Unit =
    {
        /* class initialization */
        writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "redscript/lang/RedObject", null)

        /* constructor of the main class */
        val ctor = writer.visitMethod(Opcodes.ACC_PRIVATE, "<init>", "()V", null, null)

        /* empty method, code runs in static initialization block */
        ctor.visitCode()
        ctor.visitVarInsn(Opcodes.ALOAD, 0)
        ctor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "<init>", "()V", false)
        ctor.visitInsn(Opcodes.RETURN)
        ctor.visitMaxs(0, 0)
        ctor.visitEnd()

        /* local variable scopes */
        val end = new Label
        val start = new Label

        /* generates each nodes */
        visitor.visitLabel(start)
        nodes.foreach(_.assemble(this))
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

        /* invoke the callback */
        writer.visitEnd()
        callback(mainClass, writer.toByteArray)
    }
}

object Assembler
{
    case object GetStatic
    case object GetVirtual
    case object NoSuchField

    def assemble(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit = new Assembler(mainClass, nodes, true, callback).assemble()
    def assembleInner(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit = new Assembler(mainClass, nodes, false, callback).assemble()

    def injectClass(name: String, bytecodes: Array[Byte]): Class[_] =
    {
        val loader = getClass.getClassLoader
        val injector = classOf[ClassLoader].getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

        injector.setAccessible(true)
        injector.invoke(loader, name, bytecodes, 0: Integer, bytecodes.length: Integer).asInstanceOf[Class[_]]
    }
}
