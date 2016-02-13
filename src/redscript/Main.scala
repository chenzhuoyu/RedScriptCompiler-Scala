package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """ func foo as
          |     println('in func')
          |     func nest as
          |         println('nested')
          |     end
          |     return nest
          | end
          | println('hello, world')
          | println(foo()())
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        println(s"out/$name.class ${bytecodes.length}")
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        Assembler.injectClass(name, bytecodes).getConstructor(classOf[Array[RedObject]]).newInstance(Array[RedObject]())
    })
}
