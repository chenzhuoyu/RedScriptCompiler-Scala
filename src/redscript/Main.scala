package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """ for x in {1..5} do
          |     if x == 3 then
          |         break
          |     end
          |     println(x)
          | end
          | println('hello, world')
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        Assembler.injectClass(name, bytecodes).getConstructor(classOf[Array[RedObject]]).newInstance(Array[RedObject]())
    })
}
