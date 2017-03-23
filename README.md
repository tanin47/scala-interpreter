Scala Interpreter
======================

Build your own language and API in Scala.

Define your methods with `tanin.interpreter.Api` and objects with `tanin.interpreter.Base`:

```scala
import tanin.interpreter.Api
import tanin.interpreter.Base

class Element(val id: String) extends Base {
  @Api
  def friend(friendId: String): Element = {
    new Element(s"$id.$friendId")
  }

  @Api
  override def toString(): String = {
    s"Element($id)"
  }
}

class Global {
  @Api
  def println(s: String): Unit = {
    scala.Predef.println(s)
  }

  @Api
  def printElem(elem: Element): Unit = {
    scala.Predef.println(elem.toString())
  }

  @Api
  def getElem(selector: String): Element = {
    new TestElement(selector)
  }
}
```

Execute your code:

```scala
import tanin.interpreter.Parser
import tanin.interpreter.Executor

val parser = new Parser
val executor = new Executor(new Global)
val text =
  """
    |test = "hello"
    |println(test)
    |println(getElem("first").toString())
    |printElem(getElem("first"))
    |printElem(getElem("first").friend("second"))
  """.stripMargin
val script = parser.parse(parser.script, text)
executor.run(script.get)
```

The output is:

```
hello
Element(first)
Element(first)
Element(first.second)
```


Language Feature
------------------

* Support string and object types
* Declare variables
* Invoke methods with return type


Author
--------
Tanin Na Nakorn