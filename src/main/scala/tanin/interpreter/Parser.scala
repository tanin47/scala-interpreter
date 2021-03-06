package tanin.interpreter

import scala.util.parsing.combinator.RegexParsers


sealed trait Expr

sealed trait Value extends Expr

case class StringVal(value: String) extends Value
case class IntVal(value: Int) extends Value
case class VoidVal() extends Value
case class Identifier(value: String) extends Expr

abstract class RefVal extends Value {
  def get(key: String): Value
}
abstract class MethodVal(val name: String) extends Value {
  def apply(positionalArgs: Seq[() => Value], namedArgs: Map[Identifier, () => Value]): Value
}

sealed abstract class Args
case class PositionalArgs(expr: Expr, nextOpt: Option[Args]) extends Args
case class NamedArgs(identifier: Identifier, expr: Expr, nextOpt: Option[NamedArgs]) extends Args

case class Invoke(name: Identifier, argsOpt: Option[Args]) extends Expr
case class Chain(expr: Expr, nextOpt: Option[Chain]) extends Expr
case class Declare(name: Identifier, value: Expr) extends Expr
case class NewLine() extends Expr

class Parser extends RegexParsers {
  def string: Parser[StringVal] = """"[^"]*"""".r ^^ { s => StringVal(s.substring(1, s.length - 1)) }
  def multilineString: Parser[StringVal] = "(?s)\"\"\"((?!\"\"\")[\\s\\S])*\"\"\"".r ^^ { s => StringVal(s.substring(3, s.length - 3)) }
  def int: Parser[IntVal] = "[0-9]+".r ^^ { s => IntVal(s.toInt) }

  def newLine: Parser[NewLine] = "[ ]*\n".r ^^ { _ => NewLine() }

  def value: Parser[Value] = (int | multilineString | string) ^^ identity
  def expr: Parser[Expr] = (chain | value) ^^ identity
  def namedArgs: Parser[NamedArgs] = identifier ~ "=" ~expr ~ ("," ~ namedArgs).? ^^ {
    case identifier ~ _ ~ value ~ Some(_ ~ next) => NamedArgs(identifier, value, Some(next))
    case identifier ~ _ ~ value ~ None => NamedArgs(identifier, value, None)
  }
  def positionalArgs: Parser[PositionalArgs] = expr ~ ("," ~ (namedArgs | positionalArgs)).? ^^ {
    case first ~ Some(_ ~ next) => PositionalArgs(first, Some(next))
    case first ~ None => PositionalArgs(first, None)
  }
  def args: Parser[Args] = (namedArgs | positionalArgs)  ^^ identity

  def identifier: Parser[Identifier] = "[a-zA-Z][0-9a-zA-Z-_]*".r ^^ Identifier.apply
  def invoke: Parser[Invoke] = identifier ~ "(" ~ args.? ~ ")" ^^ { case func ~ _ ~ argsOpt ~ _ => Invoke(func, argsOpt) }

  def chain: Parser[Chain] = (invoke | identifier) ~ ("." ~ chain).? ^^ {
    case first ~ None => Chain(first, None)
    case first ~ Some(_ ~ invoke) => Chain(first, Some(invoke))
  }

  def declare: Parser[Declare] = identifier ~ "=" ~ expr ^^ { case name ~ _ ~ rhs => Declare(name, rhs) }

  def instruction: Parser[Expr] = (declare | chain) ^^ identity

  def script: Parser[Seq[Expr]] = {
    phrase(rep1(instruction | newLine)) ^^ { exprsOrNewLines => exprsOrNewLines.filterNot(_.isInstanceOf[NewLine]) }
  }
}

