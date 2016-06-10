package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """ import redscript.lang.RedObject
          | class FooClass
          |     def __init__(self, x) as
          |         self.x_value = x
          |     end
          |     def test(self) as
          |         println(self.x_value)
          |     end
          |     def __getattr__(self, item) as
          |         println(item)
          |         return RedObject.__getattr__(self, item + "_value")
          |     end
          | end
          | f = FooClass(123)
          | println(f.x)
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
