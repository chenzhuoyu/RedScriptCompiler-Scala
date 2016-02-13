package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """ x = 3
          | select x in
          |     case 1, 5 then
          |         println('hello, world')
          |
          |     case {1..10} then
          |         println('fuck')
          |
          |     default
          |         println('shit')
          | end
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        Assembler.injectClass(name, bytecodes).getConstructor(classOf[Array[RedObject]]).newInstance(Array[RedObject]())
    })
}
