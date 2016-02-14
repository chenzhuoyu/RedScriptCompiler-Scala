package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """
          | func test(a) as
          |     func bar as
          |         func baz as
          |             return a
          |         end
          |         return baz
          |     end
          |     return bar
          | end
          | println(test(200)()())
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
