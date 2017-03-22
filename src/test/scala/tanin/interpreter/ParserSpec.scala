package tanin.interpreter

import tanin.BaseSpec

import scala.util.parsing.combinator.Parsers


class ParserSpec extends BaseSpec {

  def expectSuccess[T](result: Parsers#ParseResult[T], expected: T): Unit = {
    result should be(an[Parsers#Success[_]])
    result.asInstanceOf[Parsers#Success[T]].result should be(expected)
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
            Some(Args(
              StringVal("OK"),
              Some(Args(StringVal("YES"), Some(Args(Chain(Identifier("aaa"), None), None))))
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
            Some(Args(
              StringVal("OK"),
              Some(Args(Chain(Invoke(Identifier("call2"), Some(Args(StringVal("YES"), None))), None), Some(Args(StringVal("NO"), None))))
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
            Invoke(Identifier("test"), Some(Args(StringVal("yes"), None))),
            Some(Chain(
              Invoke(Identifier("test2"), None), None
            ))
          )))
      )
    )
  }
}
