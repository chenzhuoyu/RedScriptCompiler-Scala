package redscript

import redscript.compiler.{Assembler, Parser}
import redscript.lang.RedObject

object Main extends App
{
    val src =
        """ import java.lang.ArithmeticException as AE
          | try
          |     (1 / 0)
          | except AE as e
          |     println('hahaha', e)
          |     e.printStackTrace()
          | end
          | println('done')
        """.stripMargin

    val parser = new Parser(src)
    val astList = parser.parse

    Assembler.assemble("Test", astList, (name: String, bytecodes: Array[Byte]) =>
    {
        new java.io.FileOutputStream(new java.io.File(s"out/$name.class")).write(bytecodes)
        Assembler.injectClass(name, bytecodes).getConstructor(classOf[Array[RedObject]]).newInstance(Array[RedObject]())
    })
}
