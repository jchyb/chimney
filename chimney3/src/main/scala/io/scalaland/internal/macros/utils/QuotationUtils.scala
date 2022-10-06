package io.scalaland.chimney.internal.utils

import scala.quoted._
import scala.annotation.experimental

trait QuotationUtils {
  val topLevelQuotes: Quotes

  protected def unsafeIterator[Src: Type](using Quotes)(src: Expr[Src]): Expr[Any] = {
    import quotes.reflect._
    Type.of[Src] match
      case '[Array[t]] =>
        '{genericArrayOps[t](${src.asExprOf[Array[t]]}).iterator}
      case _ =>
        Select.unique(src.asTerm, "iterator").asExpr
  }

  protected def unsafeMap[Src: Type, U: Type, Q: Type](using Quotes)(src: Expr[Src], a: Expr[U => Q]): Expr[Any] = {
    import quotes.reflect._
    Type.of[Src] match
      case '[Array[U]] =>
        '{genericArrayOps[U](${src.asExprOf[Array[U]]}).map[Q]($a)(using scala.compiletime.summonInline)}
      case '[Map[_, _]] =>
        Type.of[Q] match
          case '[(t, s)] =>
            Select.overloaded(src.asTerm, "map", List(TypeRepr.of[t], TypeRepr.of[s]), List(a.asTerm)).asExpr
      case _ =>
        Select.overloaded(src.asTerm, "map", List(TypeRepr.of[Q]), List(a.asTerm)).asExpr
  }

  @experimental
  protected def unsafeToMap[From: Type, TargetMap: Type](using Quotes)(src: Expr[From]): Expr[TargetMap] = {
    import quotes.reflect._
    src.asTerm.tpe.baseType(TypeRepr.of[IterableOnce].typeSymbol).typeArgs.head.asType match
      case '[(t, s)] =>
        '{${src.asExprOf[IterableOnce[(t,s)]]}.toMap[t, s](scala.compiletime.summonInline[(t,s) <:< (t,s)])}.asExprOf[TargetMap]
  }

  protected def unsafeTo[T: Type, U: Type, Q: Type](using Quotes)(src: Expr[T], arg: Expr[scala.collection.compat.Factory[U, Q]]) = {
    import quotes.reflect._
    Apply(TypeApply(Select.unique(src.asTerm, "to"), List(TypeTree.of[Q])), List(arg.asTerm)).asExprOf[Q]
  }

  protected def unsafeSelectByName[T: Type, Q: Type](using Quotes)(source: Expr[T], name: String): Expr[Q] = {
    import quotes.reflect._
    Select.unique(source.asTerm, name).asExprOf[Q]
  }
}