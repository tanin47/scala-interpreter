package tanin.interpreter

import java.lang.reflect.InvocationTargetException

import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror


class Api extends StaticAnnotation

class Base


class Context(val map: mutable.Map[String, Value]) {

  def get(key: String): Value = {
    map.getOrElse(
      key,
      throw new Exception(s"Can't find $key because it doesn't exist in global context.")
    )
  }

  def put(key: String, value: Value): Value = {
    map.put(key, value)
    VoidVal()
  }
}

object Executor {

  def valueToNative(v: Value): AnyRef = v match {
    case StringVal(s) => s
    case p: ProxyRefVal[_] => p.underlying
    case other => throw new Exception(s"Can't convert $other to a Scala type.")
  }

  def nativeToValue(s: Any): Value = s match {
    case s: String => StringVal(s)
    case e: Base => new ProxyRefVal(e)
    case other => throw new Exception(s"Can't convert $other to the interpreter type.")
  }

  def getApiMethods[U](underlying: U)(implicit tag: TypeTag[U], classTag: ClassTag[U]): Seq[MethodVal] = {
    val javaMethods = underlying.getClass.getDeclaredMethods.map { m => m.getName -> m }.toMap

    currentMirror.classSymbol(underlying.getClass).toType.members.collect {
      case m: MethodSymbol => m
    }.filter { method =>
      method.annotations.exists(_.tree.tpe.finalResultType == typeOf[Api])
    }.map { method =>
      new MethodVal(method.name.toString) {
        def apply(positionalArgs: Seq[() => Value], namedArgs: Map[Identifier, () => Value]) = {
          val convertedArgs = method.paramLists.head.map(_.typeSignature).zip(positionalArgs).map {
            case (typeRef, arg) =>
              if (typeRef.typeSymbol == definitions.ByNameParamClass) {
                () => { valueToNative(arg()) }
              } else {
                valueToNative(arg())
              }
          }

          val javaMethod = javaMethods(method.name.toString)
          val defaultArgs = convertedArgs.length.until(method.paramLists.head.size).map { index =>

            println(method.paramLists.head.map(_.name.decodedName.toString))

            if (method.paramLists.head(index).typeSignature.typeSymbol == definitions.ByNameParamClass) {
              namedArgs.get(Identifier(method.paramLists.head(index).name.toString))
                .map { arg =>
                  () => valueToNative(arg())
                }.getOrElse {
                  // The default value of an argument is a method invocation that looks like fn$default$2 (the second argument).
                  val defaultMethod = javaMethods(s"${method.name.toString}$$default$$${index + 1}")
                  () => defaultMethod.invoke(underlying)
                }
            } else {
              namedArgs.get(Identifier(method.paramLists.head(index).name.toString))
                .map { arg =>
                  valueToNative(arg())
                }.getOrElse {
                  // The default value of an argument is a method invocation that looks like fn$default$2 (the second argument).
                  val defaultMethod = javaMethods(s"${method.name.toString}$$default$$${index + 1}")
                  defaultMethod.invoke(underlying)
                }
            }
          }

          val returnedValue = try {
            javaMethod.invoke(underlying, convertedArgs ++ defaultArgs: _*)
          } catch {
            case e: InvocationTargetException =>
              throw e.getCause
          }

          if (javaMethod.getReturnType == java.lang.Void.TYPE) {
            VoidVal()
          } else {
            nativeToValue(returnedValue)
          }
        }
      }
    }.toSeq
  }

  def getContext[U](underlying: U)(implicit tag: TypeTag[U], classTag: ClassTag[U]): Context = {
    val context = new Context(mutable.Map.empty[String, Value])
    getApiMethods(underlying).foreach { method =>
      context.put(method.name.toString, method)
    }
    context
  }
}


class ProxyRefVal[U <: AnyRef](val underlying: U)(implicit tag: TypeTag[U], classTag: ClassTag[U]) extends RefVal {
  private[this] val context = Executor.getContext(underlying)

  def get(key: String): Value = context.get(key)
}


class Executor[T](
  val global: T
)(
  implicit tag: TypeTag[T],
  classTag: ClassTag[T]
) {

  private[this] val globalContext = Executor.getContext(global)

  def run(exprs: Seq[Expr]): Unit = {
    exprs.foreach { expr =>
      execute(expr, None)
    }
  }

  def run(expr: Expr): Unit = {
    run(Seq(expr))
  }

  def dereference(identifier: Identifier, parentOpt: Option[Value]): Value = {
    parentOpt.map {
      case ref: RefVal => ref.get(identifier.value)
      case other => throw new Exception(s"Can't find ${identifier.value} inside $other because it's not a RefVal.")
    }.getOrElse {
      globalContext.get(identifier.value)
    }
  }

  def executeArgs(args: Args): (Seq[() => Value], Map[Identifier, () => Value]) = args match {
    case PositionalArgs(expr, nextOpt) =>
      val (positionalArgs, namedArgs) = nextOpt.map { a => executeArgs(a) }.getOrElse((Seq.empty, Map.empty[Identifier, () => Value]))
      ((() => execute(expr, None)) +: positionalArgs, namedArgs)

    case NamedArgs(identifier, expr, nextOpt) =>
      val (positionalArgs, namedArgs) = nextOpt.map { a => executeArgs(a) }.getOrElse((Seq.empty, Map.empty[Identifier, () => Value]))
      (positionalArgs, Map((identifier, () => execute(expr, None))) ++ namedArgs)
  }

  def executeInvoke(
    invoke: Invoke,
    parentOpt: Option[Value]
  ): Value = {
    val maybeMethod = dereference(invoke.name, parentOpt)

    maybeMethod match {
      case method: MethodVal =>
        val (positionalArgs, namedArgs) = invoke.argsOpt.map { a => executeArgs(a) }.getOrElse((Seq.empty, Map.empty[Identifier, () => Value]))
        method.apply(
          positionalArgs = positionalArgs,
          namedArgs = namedArgs
        )
      case other => throw new Exception(s"Can't invoke ${invoke.name.value} because it isn't a method but $other.")
    }
  }

  def executeChain(
    chain: Chain,
    parentOpt: Option[Value]
  ): Value = {
    val value = chain.expr match {
      case identifier: Identifier =>
        dereference(identifier, parentOpt)

      case invoke: Invoke =>
        executeInvoke(invoke, parentOpt)

      case other =>
        throw new Exception(s"Can't chain $other.")
    }

    chain.nextOpt.map { next =>
      execute(next, Some(value))
    }.getOrElse(value)
  }

  def execute(
    expr: Expr,
    parentOpt: Option[Value]
  ): Value = expr match {
    case v: Value => v

    case Identifier(key) =>
      globalContext.get(key)

    case Declare(identifier, value) =>
      globalContext.put(identifier.value, execute(value, None))

    case chain: Chain =>
      executeChain(chain, parentOpt)

    case other =>
      throw new Exception(s"Can't execute $other.")
  }
}
