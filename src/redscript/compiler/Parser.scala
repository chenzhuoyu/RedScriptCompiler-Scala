package redscript.compiler

import redscript.compiler.ast._

import scala.language.postfixOps
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.input.CharArrayReader

class Parser(val source: String) extends StdTokenParsers
{
    type Tokens = TokenSpace
    override val lexical = new Tokenizer

    type Names = List[Identifier]
    type Targets = List[NodeLValue]
    type Implements = List[Names]
    type Statements = List[NodeStatement]

    private lazy val intLit   : Parser[Long]   = accept("int"  , { case lexical.IntLit(value)   => value })
    private lazy val floatLit : Parser[Double] = accept("float", { case lexical.FloatLit(value) => value })

    private lazy val attribute  : Parser[Names]      = rep1sep(identifier, ".")
    private lazy val identifier : Parser[Identifier] = positioned(ident ^^ Identifier)

    /* Language Structures */

    private lazy val parseIf        : Parser[NodeIf]        = ("if" ~> parseExpr <~ "then") ~ (parseStatement +) ~ (("else" ~> (parseStatement +)) ?) <~ "end"               ^^ { case expr ~ success ~ failed      => new NodeIf(expr, success, failed) }
    private lazy val parseFor       : Parser[NodeFor]       = ("for" ~> identifier <~ "in") ~ parseExpr ~ ("do" ~> (parseStatement +) <~ "end")                              ^^ { case name ~ expr ~ body           => new NodeFor(name, expr, body) }
    private lazy val parseClass     : Parser[NodeClass]     = "class" ~> identifier ~ (parseExtends ?) ~ (parseImpls ?) ~ (parseStatement *) <~ "end"                        ^^ { case name ~ parent ~ intfs ~ body => new NodeClass(name, parent, intfs, body) }
    private lazy val parseRaise     : Parser[NodeRaise]     = "raise" ~> parseExpr                                                                                           ^^ { case expr                         => new NodeRaise(expr) }
    private lazy val parseWhile     : Parser[NodeWhile]     = ("while" ~> parseExpr <~ "do") ~ ((parseStatement *) <~ "end")                                                 ^^ { case cond ~ body                  => new NodeWhile(cond, body) }
    private lazy val parseSelect    : Parser[NodeSelect]    = ("select" ~> parseExpr <~ "in") ~ (parseCase +) ~ (("default" ~> (parseStatement +)) ?) <~ "end"               ^^ { case expr ~ cases ~ default       => new NodeSelect(expr, cases, default) }
    private lazy val parseFunction  : Parser[NodeFunction]  = "func" ~> identifier ~ (("(" ~> rep1sep(parseArgument, ",") <~ ")") ?) ~ ("as" ~> (parseStatement *) <~ "end") ^^ { case name ~ args ~ body           => new NodeFunction(name, args, body) }

    private lazy val parseImpls     : Parser[Implements]    = "(" ~> rep1sep(attribute, ",") <~ ")"
    private lazy val parseExtends   : Parser[Names]         = "extends" ~> attribute
    private lazy val parseFinally   : Parser[Statements]    = "finally" ~> (parseStatement +)

    private lazy val parseTry       : Parser[NodeTry]       = "try" ~> (parseStatement +) ~ (parseExcept *) ~ (parseFinally ?) <~ "end" ^? {
        case body ~ except ~ cleanup
            if !(except.isEmpty && cleanup.isEmpty ||
                 except.nonEmpty && except.count(_.except == null) >= 2)
        => new NodeTry(body, except, cleanup)
    } withFailureMessage "Invalid structure of exception rescure block"

    private lazy val parseCase      : Parser[NodeCase]      = (parseEOL *) ~>
        ( ("case" ~> rep1sep(parseExpr, ",") <~ "end")                       ^^ { case exprs        => new NodeCase(exprs, List()) }
        | ("case" ~> rep1sep(parseExpr, ",") <~ "then") ~ (parseStatement +) ^^ { case exprs ~ body => new NodeCase(exprs, body) }) <~ (parseEOL *)

    private lazy val parseArgument  : Parser[NodeArgument]  = (">" ?) ~ identifier                                ^^ { case variant ~ name => new NodeArgument(name, variant.isDefined) }
    private lazy val parseException : Parser[NodeException] = rep1sep(attribute, "or") ~ (("as" ~> identifier) ?) ^^ { case names ~ name   => NodeException(names, name) }

    private lazy val parseImport    : Parser[NodeImport]    =
        (("import" ~> attribute <~ "as") ~ identifier ^^ { case names ~ alias => new NodeImport(names, alias) }
        | "import" ~> attribute                       ^^ { case names         => new NodeImport(names, null) })

    private lazy val parseExcept    : Parser[NodeExcept]    = "except" ~>
        ( "*" ~> (parseStatement *)           ^^ { case body          => NodeExcept(null, body) }
        | parseException ~ (parseStatement *) ^^ { case except ~ body => NodeExcept(except, body) })

    private lazy val parseDecorator : Parser[NodeDecorator] =
        ( "@" ~> parseExpr ~ parseLambda    ^^ { case expr ~ func => new NodeDecorator(expr, Left(func)) }
        | "@" ~> parseExpr ~ parseDecorator ^^ { case expr ~ func => new NodeDecorator(expr, Right(func)) })

    /* Basic Statements */

    private lazy val parseBreak     : Parser[NodeBreak]     = "break"    ^^^ new NodeBreak
    private lazy val parseContinue  : Parser[NodeContinue]  = "continue" ^^^ new NodeContinue

    private lazy val parseReturn    : Parser[NodeReturn]    =
        ( "return" ~> (parseExpr <~ ",") ~ repsep(parseExpr, ",") ^^ { case first ~ remains => new NodeReturn(first :: remains, true)}
        | "return" ~> parseExpr                                   ^^ { case expr            => new NodeReturn(expr :: Nil, false) })

    private lazy val parseLValue    : Parser[NodeLValue]    = parseValue             ^? { case value if value.isMutable   => new NodeLValue(value) } withFailureMessage "Assign to constant"
    private lazy val parseDelete    : Parser[NodeDelete]    = "delete" ~> parseValue ^? { case value if value.isDeletable => new NodeDelete(value) } withFailureMessage "Deleting constant"

    private lazy val parseAssign    : Parser[NodeAssign]    = ((parseLValue <~ "=") +) ~ parseExpr            ^^ { case targets ~ expr  => new NodeAssign(targets, expr) }
    private lazy val parseTargets   : Parser[Targets]       = (parseLValue <~ ",") ~ repsep(parseLValue, ",") ^^ { case first ~ remains => first :: remains }

    private lazy val parseParallels : Parser[NodeParallels] =
        ( parseTargets ~ ("=" ~> parseExpr <~ ",") ~ repsep(parseExpr, ",") ^^ { case targets ~ first ~ remains => new NodeParallels(targets, first :: remains, isTuple = true) }
        | parseTargets ~ ("=" ~> parseExpr)                                 ^^ { case targets ~ expr            => new NodeParallels(targets, expr :: Nil, isTuple = false) })

    private lazy val parseIncrement : Parser[NodeIncrement] =
        ( parseLValue ~ ( "=" | "+=" | "-=" |  "*=" |  "/=" |  "%=") ~ parseExpr ^^ { case target ~ op ~ expr => new NodeIncrement(target, op, expr) }
        | parseLValue ~ ("&=" | "|=" | "^=" | "<<=" | ">>=" | ">>=") ~ parseExpr ^^ { case target ~ op ~ expr => new NodeIncrement(target, op, expr) })

    /* Generic Statement */

    private lazy val parseEOL       : Parser[Any] = "\n"
    private lazy val parseStatement : Parser[NodeStatement] = (parseEOL *) ~>
        ( parseIf        ^^ (new NodeStatement(_))
        | parseFor       ^^ (new NodeStatement(_))
        | parseTry       ^^ (new NodeStatement(_))
        | parseBreak     ^^ (new NodeStatement(_))
        | parseClass     ^^ (new NodeStatement(_))
        | parseRaise     ^^ (new NodeStatement(_))
        | parseWhile     ^^ (new NodeStatement(_))
        | parseDelete    ^^ (new NodeStatement(_))
        | parseReturn    ^^ (new NodeStatement(_))
        | parseImport    ^^ (new NodeStatement(_))
        | parseSelect    ^^ (new NodeStatement(_))
        | parseContinue  ^^ (new NodeStatement(_))
        | parseFunction  ^^ (new NodeStatement(_))
        | parseDecorator ^^ (new NodeStatement(_))
        | parseAssign    ^^ (new NodeStatement(_))
        | parseIncrement ^^ (new NodeStatement(_))
        | parseParallels ^^ (new NodeStatement(_))
        | parseValue     ^^ (new NodeStatement(_))) <~ (parseEOL *)

    /* Expressions */

    private lazy val parseName      : Parser[NodeName]      = identifier                              ^^ (new NodeName(_))
    private lazy val parseAttr      : Parser[NodeAttr]      = ("." ~> identifier)                     ^^ (new NodeAttr(_))
    private lazy val parseIndex     : Parser[NodeIndex]     = ("[" ~> parseExpr <~ "]")               ^^ (new NodeIndex(_))
    private lazy val parseInvoke    : Parser[NodeInvoke]    = ("(" ~> repsep(parseParam, ",") <~ ")") ^^ (new NodeInvoke(_))

    private lazy val parseMap       : Parser[NodeMap]       = "{" ~> repsep(parseRow , ",") <~ "}"    ^^ (new NodeMap(_))
    private lazy val parseArray     : Parser[NodeArray]     = "[" ~> repsep(parseExpr, ",") <~ "]"    ^^ (new NodeArray(_))

    private lazy val parsePair      : Parser[NodePair]      = identifier ~ ("->" ~> parseExpr)                                                               ^^ { case key   ~ value   => new NodePair(key, value) }
    private lazy val parseLambda    : Parser[NodeFunction]  = "$" ~> (("(" ~> rep1sep(parseArgument, ",") <~ ")") ?) ~ ("->" ~> (parseStatement *) <~ "end") ^^ { case args ~ body     => new NodeFunction(null, args, body) }

    private lazy val parseTuple     : Parser[NodeTuple]     =
        ( "(" ~> ")"                                                ^^ { case _               => new NodeTuple(List()) }
        | "(" ~> (parseExpr <~ ",") ~ repsep(parseExpr, ",") <~ ")" ^^ { case first ~ remains => new NodeTuple(first :: remains) })

    private lazy val parseExpr      : Parser[NodeExpr]      = parseBoolOr  * ("in" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "in", y)})
    private lazy val parseBoolOr    : Parser[NodeExpr]      = parseBoolAnd * ("||" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "||", y)})
    private lazy val parseBoolAnd   : Parser[NodeExpr]      = parseBitOr   * ("&&" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "&&", y)})
    private lazy val parseBitOr     : Parser[NodeExpr]      = parseBitXor  * ("|"  ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "|" , y)})
    private lazy val parseBitXor    : Parser[NodeExpr]      = parseBitAnd  * ("^"  ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "^" , y)})
    private lazy val parseBitAnd    : Parser[NodeExpr]      = parseEquals  * ("&"  ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "&" , y)})

    private lazy val parseEquals    : Parser[NodeExpr]      = parseCompares *
        ( "!=" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "!=", y)}
        | "==" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "==", y)})

    private lazy val parseCompares  : Parser[NodeExpr]      = parseShifts *
        ( "<"  ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "<" , y)}
        | ">"  ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), ">" , y)}
        | "<=" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "<=", y)}
        | ">=" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), ">=", y)})

    private lazy val parseShifts    : Parser[NodeExpr]      = parseAddSub *
        ( "<<" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "<<", y)}
        | ">>" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), ">>", y)})

    private lazy val parseAddSub    : Parser[NodeExpr]      = parseTerm *
        ( "+" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "+", y)}
        | "-" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "-", y)})

    private lazy val parseTerm      : Parser[NodeExpr]      = parsePower *
        ( "*" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "*", y)}
        | "/" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "/", y)}
        | "%" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "%", y)})

    private lazy val parsePower     : Parser[NodeExpr]      = parseFactor * ("**" ^^^ {(x: NodeExpr, y: NodeExpr) => new NodeExpr(Left(x), "**", y)})
    private lazy val parseFactor    : Parser[NodeExpr]      =
        ( "+" ~> parseFactor      ^^ (new NodeExpr(null, "+", _))
        | "-" ~> parseFactor      ^^ (new NodeExpr(null, "-", _))
        | "~" ~> parseFactor      ^^ (new NodeExpr(null, "~", _))
        | "!" ~> parseFactor      ^^ (new NodeExpr(null, "!", _))
        | "(" ~> parseExpr <~ ")"
        | parseValue              ^^ { value => new NodeExpr(Right(value), null, null) })

    private lazy val parseRange     : Parser[NodeRange]     = "{" ~> parseExpr ~ (".." ~> parseExpr) <~ "}" ^^ { case lower ~ upper => new NodeRange(lower, upper) }
    private lazy val parseConst     : Parser[NodeConst]     =
        ( intLit    ^^ (new NodeConst(_))
        | floatLit  ^^ (new NodeConst(_))
        | stringLit ^^ (new NodeConst(_)))

    private lazy val parseRow       : Parser[NodeRow]       = parseExpr ~ (":" ~> parseExpr) ^^ { case key ~ value   => new NodeRow(key, value) }
    private lazy val parseParam     : Parser[NodeParam]     = (">" ?) ~ parseExpr            ^^ { case expand ~ expr => new NodeParam(expr, expand = expand.isDefined) }

    private lazy val parseValue     : Parser[NodeValue]     =
        ( parseMap
        | parsePair
        | parseArray
        | parseConst
        | parseRange
        | parseTuple
        | parseLambda
        | parseName
        | "(" ~> parseExpr <~ ")"
        ) ~ (
        ( parseAttr   ^^ { value => Left(value) }
        | parseIndex  ^^ { value => Right(Left(value)) }
        | parseInvoke ^^ { value => Right(Right(value)) }) *
        ) ^^ { case value ~ modifiers => new NodeValue(value, modifiers) }

    /* Compiler */

    private lazy val script: String = source + CharArrayReader.EofCh
    private lazy val parseProgram: Parser[Statements] = (parseStatement +) <~ lexical.EOF

    def parse: Statements = parseProgram(new lexical.Scanner(script)) match
    {
        case Success(result, _)       => result
        case NoSuccess(message, next) => throw new SyntaxError(message, next.pos.line, next.pos.column)
    }
}
