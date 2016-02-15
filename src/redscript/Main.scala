package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """
          | func foo(a, >b) as
          |     println('test', a, b)
          | end
          | foo(1)
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        System.err.println(s"Assembling out/$name.class, size ${bytecodes.length}")
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        Assembler.injectClass(name, bytecodes)
    })

    Class.forName("Test").getConstructor(classOf[Array[RedObject]]).newInstance(Array[RedObject]())
}
