package tanin.interpreter

import tanin.BaseSpec

import scala.util.parsing.combinator.Parsers


class ParserSpec extends BaseSpec {

  def expectSuccess[T](result: Parsers#ParseResult[T], expected: T): Unit = {
    result should be(an[Parsers#Success[_]])
    result.asInstanceOf[Parsers#Success[T]].result should be(expected)
  }

  it("parses named args") {
    val parser = new Parser

    expectSuccess(
      parser.parse(parser.script,"""test(first = "hello")"""),
      Seq(
        Chain(
          Invoke(
            Identifier("test"),
            Some(NamedArgs(Identifier("first"), StringVal("hello"), None))),
          None)
      )
    )
    expectSuccess(
      parser.parse(parser.script, """test("hello", second = "hello2")"""),
      Seq(
        Chain(
          Invoke(
            Identifier("test"),
            Some(
              PositionalArgs(
                StringVal("hello"),
                Some(NamedArgs(Identifier("second"), StringVal("hello2"), None))))),
          None
        )
      )
    )
  }

  it("parses Declare.") {
    val parser = new Parser

    expectSuccess(
      parser.parse(parser.script, "test = \"\"\"hel\nlo\"\"\""),
      Seq(Declare(Identifier("test"), StringVal("hel\nlo")))
    )
    expectSuccess(
      parser.parse(parser.script, """test = "hello""""),
      Seq(Declare(Identifier("test"), StringVal("hello")))
    )
  }

  it("parses Invoke with no arguments.") {
    val parser = new Parser
    expectSuccess(
      parser.parse(parser.script, """call()"""),
      Seq(Chain(Invoke(Identifier("call"), None), None))
    )
  }

  it("parses Invoke with multiple arguments.") {
    val parser = new Parser
    expectSuccess(
      parser.parse(parser.script, """call("OK", "YES", aaa)"""),
      Seq(
        Chain(
          Invoke(
            Identifier("call"),
            Some(PositionalArgs(
              StringVal("OK"),
              Some(PositionalArgs(StringVal("YES"), Some(PositionalArgs(Chain(Identifier("aaa"), None), None))))
            ))
          ),
          None
        )
      )
    )
  }

  it("parses nested Invokes.") {
    val parser = new Parser
    expectSuccess(
      parser.parse(parser.script, """call("OK", call2("YES"), "NO")"""),
      Seq(
        Chain(
          Invoke(
            Identifier("call"),
            Some(PositionalArgs(
              StringVal("OK"),
              Some(PositionalArgs(Chain(Invoke(Identifier("call2"), Some(PositionalArgs(StringVal("YES"), None))), None), Some(PositionalArgs(StringVal("NO"), None))))
            ))
          ),
          None
        )
      )
    )
  }

  it("parses Chain with multiple Invoke.") {
    val parser = new Parser
    expectSuccess(
      parser.parse(parser.script, """call.test("yes").test2()"""),
      Seq(
        Chain(
          Identifier("call"),
          Some(Chain(
            Invoke(Identifier("test"), Some(PositionalArgs(StringVal("yes"), None))),
            Some(Chain(
              Invoke(Identifier("test2"), None), None
            ))
          )))
      )
    )
  }
}
