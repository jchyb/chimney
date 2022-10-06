package io.scalaland.chimney.internal.macros

import scala.annotation.experimental
import scala.quoted._

trait Model extends TransformerConfigSupport {

  val topLevelQuotes: Quotes

  case class Target(name: String, tpe: Type[Any]) {
    override def equals(other: Any): Boolean = // workaround for the same Type[Any] not being equal
      other match {
        case Target(otherName, otherTpe) =>
          import topLevelQuotes.reflect._
          given Quotes = topLevelQuotes
          name == otherName && (
          (tpe, otherTpe) match {
            case ('[t1], '[t2]) =>
              TypeRepr.of[t1] == TypeRepr.of[t2]
          })
        case _ => false 
      }
  }
  object Target {
    // def fromJavaBeanSetter(ms: MethodSymbol, site: Type): Target =
    //   Target(ms.canonicalName, ms.beanSetterParamTypeIn(site))

    @experimental
    def fromField[T: Type](ms: topLevelQuotes.reflect.Symbol): Target = {
      Target(ms.name, resultTypeIn[T](using topLevelQuotes)(ms))
    }
  }

  case class TransformerBodyTree(tree: Expr[Any], target: DerivationTarget) {
    def isTotalTarget: Boolean = target == DerivationTarget.TotalTransformer
    def isLiftedTarget: Boolean = false // target.isInstanceOf[DerivationTarget.LiftedTransformer]
  }

  sealed trait AccessorResolution extends Product with Serializable {
    def isResolved: Boolean
  }
  object AccessorResolution {
    case object NotFound extends AccessorResolution {
      override def isResolved: Boolean = false
    }
    case class Resolved(symbol: topLevelQuotes.reflect.Symbol, wasRenamed: Boolean) extends AccessorResolution {
      override def isResolved: Boolean = true
    }
    case object DefAvailable extends AccessorResolution {
      override def isResolved: Boolean = false
    }
  }
}
