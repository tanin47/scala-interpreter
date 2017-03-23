package tanin.interpreter

import tanin.BaseSpec


class TestElement(val selector: String) extends Base {
  @Api
  def child(childSelector: String): TestElement = {
    new TestElement(s"$selector $childSelector")
  }

  @Api
  override def toString(): String = {
    s"TestElement($selector)"
  }
}


class TestGlobal {
  @Api
  def println(s: String): Unit = {
    scala.Predef.println(s)
  }

  @Api
  def printElem(elem: TestElement): Unit = {
    scala.Predef.println(elem.toString())
  }

  @Api
  def getElem(selector: String): TestElement = {
    new TestElement(selector)
  }

  @Api
  def getSomeString(s: String): String = {
    s"$s-some-string"
  }

  @Api
  def raiseException(): Unit = {
    throw new Exception("fake exception")
  }
}


class ExecutorSpec extends BaseSpec {

  it("executes Declare.") {
    val parser = new Parser
    val executor = new Executor(new TestGlobal)
    val text =
      """
        |test = "hello"
        |println(test)
        |println(getSomeString("s"))
        |println(getSomeString(test))
        |println(getElem("someElem").toString())
        |printElem(getElem("someElem2"))
        |printElem(getElem("someElem3").child("child"))
      """.stripMargin
    val script = parser.parse(parser.script, text)
    executor.run(script.get)
  }

  it("handles exception") {
    val parser = new Parser
    val executor = new Executor(new TestGlobal)
    val script = parser.parse(parser.script, "raiseException()")

    val exception = intercept[Exception]{ executor.run(script.get) }
    exception.getMessage should be("fake exception")
  }
}