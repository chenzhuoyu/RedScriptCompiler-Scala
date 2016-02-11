package redscript.compiler

import scala.language.postfixOps
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._

class Tokenizer extends StdLexical with TokenSpace
{
    reserved.clear()
    delimiters.clear()

    reserved ++= List(
        "as",
        "do",
        "if",
        "in",
        "end",
        "for",
        "try",
        "case",
        "else",
        "func",
        "then",
        "break",
        "class",
        "raise",
        "while",
        "delete",
        "except",
        "import",
        "return",
        "select",
        "default",
        "extends",
        "finally",
        "continue"
    )

    delimiters ++= List(
        "("  , ")"  , "[" , "]" , "{" , "}" ,
        "->" ,
        "==" , "!=" , "=" ,
        "&&" , "||" , "!" ,
        "&=" , "|=" , "^=",
        "&"  , "|"  , "^" , "~" ,
        "<<=", ">>=", "<=", ">=",
        "<<" , ">>" , "<" , ">" ,
        "**=", "+=" , "-=", "*=", "/=", "%=",
        "**" , "+"  , "-" , "*" , "/" , "%" ,
        ".." , "."  , "," ,
        ":"  , "@"  , "`" , "$" ,
        "\n"
    )

    private def stringOf(p: => Parser[Char]): Parser[String] = rep1(p) ^^ (_ mkString "")

    private lazy val octalDigit      : Parser[Long] = elem("octal digit" , c => '0' <= c && c <= '7') ^^ (_ - '0')
    private lazy val binaryDigit     : Parser[Long] = elem("binary digit", c => c == '0' || c == '1') ^^ (_ - '0')
    private lazy val hexadecimalDigit: Parser[Long] = accept("hexadecimal digit",
    {
        case c @ ('a' | 'b' | 'c' | 'd' | 'e' | 'f')                         => c - 'a' + 10
        case c @ ('A' | 'B' | 'C' | 'D' | 'E' | 'F')                         => c - 'A' + 10
        case c @ ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') => c - '0'
    })

    private lazy val escape: Parser[Char] = '\\' ~>
        ( octalDigit ~ octalDigit ~ octalDigit                     ^^ { case a ~ b ~ c => (a * 64 + b * 8 + c).toChar }
        | (elem('x') | 'X') ~> hexadecimalDigit ~ hexadecimalDigit ^^ { case a ~ b     => (a * 16 + b        ).toChar }
        | '\\'
        | '\"'
        | '\''
        | 'b' ^^^ '\b'
        | 't' ^^^ '\t'
        | 'n' ^^^ '\n'
        | 'r' ^^^ '\r')

    private lazy val identifier: Parser[Token] =
        ( identChar ~ ((identChar | digit) *)           ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
        | '`' ~> (chrExcept('`', '\n', EofCh) *) <~ '`' ^^ { case ident        => processIdent(ident mkString "") }
        | '`' ~> failure("Unclosed qualified identifier"))

    private lazy val numberLiteral: Parser[Token] =
        ( '.' ~> stringOf(digit)                              ^^ { case       fract => FloatLit(   s"0.$fract".toFloat) }
        | stringOf(digit) ~ ('.' ~> stringOf(digit))          ^^ { case int ~ fract => FloatLit(s"$int.$fract".toFloat) }
        | ('0' ~ (elem('b') | 'B')) ~> rep1(binaryDigit)      ^^ { value => IntLit(value.foldLeft(0L)(_ *  2 + _)) }
        | ('0' ~ (elem('x') | 'X')) ~> rep1(hexadecimalDigit) ^^ { value => IntLit(value.foldLeft(0L)(_ * 16 + _)) }
        | '0' ~> rep1(octalDigit)                             ^^ { value => IntLit(value.foldLeft(0L)(_ *  8 + _)) }
        | stringOf(digit)                                     ^^ { value => IntLit(value.toLong) })

    private lazy val stringLiteral: Parser[String] =
        ( '\'' ~> '\''  ^^ { case _ => "" }
        | '\"' ~> '\"'  ^^ { case _ => "" }
        | '\'' ~> stringOf(chrExcept('\\', '\'', EofCh) | escape) <~ '\''
        | '\"' ~> stringOf(chrExcept('\\', '\"', EofCh) | escape) <~ '\"'
        | '\'' ~> failure("Unclosed string literal")
        | '\"' ~> failure("Unclosed string literal"))

    override def token: Parser[Token] =
        ( whitespace ~> identifier    <~ whitespace
        | whitespace ~> numberLiteral <~ whitespace
        | whitespace ~> stringLiteral <~ whitespace ^^  StringLit
        | EofCh                                     ^^^ EOF
        | delim
        | failure("Illegal character"))

    override def whitespace: Parser[Any] = (whitespaceChar | ('#' ~ (chrExcept('\n', EofCh) *))) *
    override def whitespaceChar: Parser[Char] = elem("whitespace char", c => c != '\n' && c.isSpaceChar)
    override protected def processIdent(name: String) = if (reserved contains name) Keyword(name) else Identifier(name)
}
