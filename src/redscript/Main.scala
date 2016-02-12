package redscript

import redscript.compiler.{Assembler, Parser}

object Main extends App
{
    val src =
        """ for x in {1..5} do
          |     println(x)
          | end
          | println('hello, world')
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        val ctor = Assembler.injectClass(name, bytecodes).getDeclaredConstructor()

        ctor.setAccessible(true)
        ctor.newInstance()
    })
}
