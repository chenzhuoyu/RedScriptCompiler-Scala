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

    val builtins = Map(
        "print"    -> "print",
        "println"  -> "println",
        "readline" -> "readline"
    )
}
