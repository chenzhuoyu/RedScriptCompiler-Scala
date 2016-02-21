package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """
          | class FooClass
          |     x = 100
          |     def test(self) as
          |         return 1
          |     end
          |     def __init__(self) as
          |         println(self)
          |     end
          |     def __getattr__(self, name) as
          |         return 1
          |     end
          |     def __invoke__(self, a, b) as
          |         println(a, b)
          |     end
          | end
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
