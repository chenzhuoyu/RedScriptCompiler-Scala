package redscript

import java.lang.reflect.InvocationTargetException

import redscript.compiler.{Assembler, Parser}

object Main extends App
{
    val src =
        """ x = {1: 2}
          | println(x)
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        new java.io.FileOutputStream(new java.io.File(s"$name.class")).write(bytecodes)
        val mainMethod = Assembler.injectClass(name, bytecodes).getMethod("main", classOf[Array[String]])

        try
        {
            mainMethod.invoke(null, args)
        } catch
        {
            case e: InvocationTargetException =>
                throw e.getCause
        }
    })
}
