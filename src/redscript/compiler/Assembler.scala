package redscript.compiler

import org.objectweb.asm._
import org.objectweb.asm.commons.LocalVariablesSorter
import redscript.compiler.ast.Node
import redscript.lang.RedObject

import scala.collection.mutable

class Assembler private(mainClass: String, nodes: List[Node], val callback: (String, Array[Byte]) => Unit)
{
    val name = mainClass.replace('.', '/')
    val locals = mutable.Map[String, Int]()
    val fields = mutable.Map[String, String]()
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    val visitor = new LocalVariablesSorter(Opcodes.ACC_PUBLIC, "([Ljava/lang/String;)V", writer.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "([Ljava/lang/String;)V", null, null))

    def getLocal(local: String) = locals.get(local)
    def makeLocal(local: String) = locals.getOrElseUpdate(local, visitor.newLocal(Type.getType(classOf[RedObject])))

    def hasField(field: String) = fields.contains(field)
    def makeField(field: String, desc: String) =
    {
        fields(field) = desc
        writer.visitField(Opcodes.ACC_PUBLIC, field, desc, null, null)
    }

    def assemble(): Unit =
    {
        /* class initialization */
        writer.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC, name, null, "redscript/lang/RedObject", null)

        /* main entry point of any Java applications */
        val entryPoint = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "main", "([Ljava/lang/String;)V", null, null)

        /* simply create an instance of the main class, it will take over */
        entryPoint.visitCode()
        entryPoint.visitTypeInsn(Opcodes.NEW, name)
        entryPoint.visitVarInsn(Opcodes.ALOAD, 0)
        entryPoint.visitMethodInsn(Opcodes.INVOKESPECIAL, name, "<init>", "([Ljava/lang/String;)V", false)
        entryPoint.visitInsn(Opcodes.RETURN)
        entryPoint.visitMaxs(0, 0)
        entryPoint.visitEnd()

        /* first, we need to invoke the super constructor */
        visitor.visitCode()
        visitor.visitVarInsn(Opcodes.ALOAD, 0)
        visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, "redscript/lang/RedObject", "<init>", "()V", false)

        /* local variable scopes */
        val end = new Label
        val start = new Label

        /* generates each nodes */
        visitor.visitLabel(start)
        nodes foreach (_.assemble(this))
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
    def assemble(mainClass: String, nodes: List[Node], callback: (String, Array[Byte]) => Unit): Unit = new Assembler(mainClass, nodes, callback).assemble()
    def injectClass(name: String, bytecodes: Array[Byte]): Class[_] =
    {
        val loader = getClass.getClassLoader
        val injector = classOf[ClassLoader].getDeclaredMethod("defineClass", classOf[String], classOf[Array[Byte]], classOf[Int], classOf[Int])

        injector.setAccessible(true)
        injector.invoke(loader, name, bytecodes, 0: Integer, bytecodes.length: Integer).asInstanceOf[Class[_]]
    }
}
