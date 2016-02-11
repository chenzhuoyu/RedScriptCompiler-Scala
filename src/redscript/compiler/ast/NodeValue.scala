package redscript.compiler.ast

import redscript.compiler.Assembler

class NodeValue(val value: Node, val modifiers: List[Either[NodeAttr, Either[NodeIndex, NodeInvoke]]]) extends Node
{
    def isMutable: Boolean = value match
    {
        case _: NodeName => modifiers.isEmpty || (modifiers.last match
        {
            case Left(_)         => true
            case Right(Left(_))  => true
            case Right(Right(_)) => false
        })

        case _           => modifiers.nonEmpty && (modifiers.last match
        {
            case Left(_)         => true
            case Right(Left(_))  => true
            case Right(Right(_)) => false
        })
    }

    def isDeletable: Boolean = modifiers.nonEmpty && (modifiers.last match
    {
        case Left(_)         => true
        case Right(Left(_))  => true
        case Right(Right(_)) => false
    })

    override def assemble(assembler: Assembler): Unit =
    {
        value.assemble(assembler)
        modifiers foreach {
            case Left(x)         => x.assemble(assembler)
            case Right(Left(x))  => x.assemble(assembler)
            case Right(Right(x)) => x.assemble(assembler)
        }
    }
}
