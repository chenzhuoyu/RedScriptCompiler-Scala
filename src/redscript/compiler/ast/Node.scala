package redscript.compiler.ast

import redscript.compiler.Assembler

import scala.util.parsing.input.Positional

trait Node
{
    def assemble(method: Assembler): Unit
}

case class Identifier(value: String) extends Positional
{
    override def toString: String = value
}
