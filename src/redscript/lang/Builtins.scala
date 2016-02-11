package redscript.lang

import scala.io.StdIn

object Builtins
{
    val print = new RedObject
    {
        override def __invoke__(args: Array[RedObject]): RedObject =
        {
            if (args.isEmpty)
                return RedNull.Null

            Predef.print(args map (_.__str__) mkString " ")
            RedNull.Null
        }
    }

    val println = new RedObject
    {
        override def __invoke__(args: Array[RedObject]): RedObject =
        {
            if (args.isEmpty)
            {
                Predef.println()
                return RedNull.Null
            }

            Predef.println(args map (_.__str__) mkString " ")
            RedNull.Null
        }
    }

    val readline = new RedObject
    {
        override def __invoke__(args: Array[RedObject]): RedObject =
        {
            if (args.isEmpty)
                return new RedString(StdIn.readLine())

            if (args.length != 1)
                throw new ArgumentError("'readline' takes at most 1 string argument")

            scala.Console.err.println(args.head.__str__)
            new RedString(StdIn.readLine())
        }
    }

    def Class: RedObject = new RedJavaClass(classOf[Class[_]])
    def System: RedObject = new RedJavaClass(classOf[System])
    def Console: RedObject = new RedJavaObject(scala.Console)

    val builtins = Map(
        "print"    -> "print",
        "println"  -> "println",
        "readline" -> "readline",

        "Class"   -> "Class",
        "System"  -> "System",
        "Console" -> "Console"
    )
}
