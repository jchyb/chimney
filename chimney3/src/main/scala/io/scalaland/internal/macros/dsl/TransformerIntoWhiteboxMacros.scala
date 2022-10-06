package io.scalaland.chimney.internal.macros.dsl

// import io.scalaland.chimney.internal.utils.DslMacroUtils

// import scala.annotation.unused
// import scala.reflect.macros.whitebox

// class TransformerIntoWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

//   import c.universe._

//   def withFieldConstImpl(selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
//     c.prefix.tree.refineTransformerDefinition_Hack(
//       trees => q"_.withFieldConst($selector, ${trees("value")})",
//       "value" -> value
//     )
//   }

//   def withFieldConstFImpl[F[+_]](selector: Tree, value: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix.tree}.lift[$F].withFieldConstF($selector, $value)"
//   }

//   def withFieldComputedImpl(selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
//     c.prefix.tree.refineTransformerDefinition_Hack(
//       trees => q"_.withFieldComputed($selector, ${trees("f")})",
//       "f" -> f
//     )
//   }

//   def withFieldComputedFImpl[F[+_]](selector: Tree, f: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix.tree}.lift[$F].withFieldComputedF($selector, $f)"
//   }

//   def withFieldRenamedImpl(selectorFrom: Tree, selectorTo: Tree): Tree = {
//     c.prefix.tree.refineTransformerDefinition(q"_.withFieldRenamed($selectorFrom, $selectorTo)")
//   }

//   def withCoproductInstanceImpl(f: Tree): Tree = {
//     c.prefix.tree.refineTransformerDefinition_Hack(
//       trees => q"_.withCoproductInstance(${trees("f")})",
//       "f" -> f
//     )
//   }

//   def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix.tree}.lift[$F].withCoproductInstanceF($f)"
//   }
// }

import io.scalaland.chimney.dsl.TransformerInto
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.dsl.TransformerDefinition

import scala.quoted._
import scala.annotation.unused

object TransformerIntoWhiteboxMacros {
  // changed not to use quotations, as type resolution went bad there when using transparenbt inline, instead we call methods directly
  // TODO generalise
  def withFieldConstImpl[From: Type, To: Type, C <: TransformerCfg: Type, T: Type, U: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(
    ti: Expr[TransformerInto[From, To, C, Flagss]], selector: Expr[To => T], value: Expr[U], ev: Expr[U <:< T]) = {
    import quotes.reflect._

    val a = new TransformerDefinitionWhiteboxMacros(quotes)
          .withFieldConstImpl[C, From, To, T, U, Flagss]('{$ti.td}, selector, value)
    
    a.asExprOf[Any] match {
      case '{$x: io.scalaland.chimney.dsl.TransformerDefinition[_, _, t, _]} =>
        Apply(TypeApply(Select.unique(New(TypeTree.of[TransformerInto]), "<init>"), List(TypeTree.of[From], TypeTree.of[To], TypeTree.of[t], TypeTree.of[Flagss])), List('{$ti.source}.asTerm, a.asTerm))
          .asExprOf[io.scalaland.chimney.dsl.TransformerInto[From, To, t, Flagss]]
    }
  }

  def withFieldComputedImpl[From: Type, To: Type, C <: TransformerCfg: Type, T: Type, U: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(
    ti: Expr[TransformerInto[From, To, C, Flagss]], selector: Expr[To => T], f: Expr[From => U], ev: Expr[U <:< T]) = {
    import quotes.reflect._

    val a = new TransformerDefinitionWhiteboxMacros(quotes)
          .withFieldComputedImpl[C, From, To, T, U, Flagss]('{$ti.td}, selector, f)
    
    a.asExprOf[Any] match {
      case '{$x: io.scalaland.chimney.dsl.TransformerDefinition[_, _, t, _]} =>
        Apply(TypeApply(Select.unique(New(TypeTree.of[TransformerInto]), "<init>"), List(TypeTree.of[From], TypeTree.of[To], TypeTree.of[t], TypeTree.of[Flagss])), List('{$ti.source}.asTerm, a.asTerm))
          .asExprOf[io.scalaland.chimney.dsl.TransformerInto[From, To, t, Flagss]]
    }
  }

  def withFieldRenamedImpl[From: Type, To: Type, C <: TransformerCfg: Type, T: Type, U: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(ti: Expr[TransformerInto[From, To, C, Flagss]], selectorFrom: Expr[From => T], selectorTo: Expr[To => U]) = {
    import quotes.reflect._

    val a = new TransformerDefinitionWhiteboxMacros(quotes)
      .withFieldRenamedImpl[C, From, To, T, U, Flagss]('{$ti.td}, selectorFrom, selectorTo)

    a.asExprOf[Any] match {
      case '{$x: io.scalaland.chimney.dsl.TransformerDefinition[_, _, t, _]} =>
        Apply(TypeApply(Select.unique(New(TypeTree.of[TransformerInto]), "<init>"), List(TypeTree.of[From], TypeTree.of[To], TypeTree.of[t], TypeTree.of[Flagss])), List('{$ti.source}.asTerm, a.asTerm))
          .asExprOf[io.scalaland.chimney.dsl.TransformerInto[From, To, t, Flagss]]
    }
  }

  def withCoproductInstanceImpl[From: Type, Inst: Type, To: Type, C <: TransformerCfg: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(
    ti: Expr[TransformerInto[From, To, C, Flagss]], f: Expr[Inst => To]) = {
    import quotes.reflect._

    val a = new TransformerDefinitionWhiteboxMacros(quotes)
      .withCoproductInstanceImpl[C, From, To, Inst, Flagss]('{$ti.td}, f)

    a.asExprOf[Any] match {
      case '{$x: io.scalaland.chimney.dsl.TransformerDefinition[_, _, t, _]} =>
        Apply(TypeApply(Select.unique(New(TypeTree.of[TransformerInto]), "<init>"), List(TypeTree.of[From], TypeTree.of[To], TypeTree.of[t], TypeTree.of[Flagss])), List('{$ti.source}.asTerm, a.asTerm))
          .asExprOf[io.scalaland.chimney.dsl.TransformerInto[From, To, t, Flagss]]
    }
  }
}
