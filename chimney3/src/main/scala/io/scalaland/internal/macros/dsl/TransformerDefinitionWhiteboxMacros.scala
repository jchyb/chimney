package io.scalaland.chimney.internal.macros.dsl

// import scala.annotation.unused
// import scala.reflect.macros.whitebox

// class TransformerDefinitionWhiteboxMacros(val c: whitebox.Context) extends DslMacroUtils {

//   import CfgTpes._
//   import c.universe._

//   def withFieldConstImpl[C: WeakTypeTag](selector: Tree, value: Tree)(@unused ev: Tree): Tree = {
//     c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, value, fieldConstT)
//   }

//   def withFieldConstFImpl[F[+_]](selector: Tree, value: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix}.lift[$F].withFieldConstF($selector, $value)"
//   }

//   def withFieldComputedImpl[C: WeakTypeTag](selector: Tree, f: Tree)(@unused ev: Tree): Tree = {
//     c.prefix.tree.overrideField[C](selector.extractSelectorFieldName, f, fieldComputedT)
//   }

//   def withFieldComputedFImpl[F[+_]](selector: Tree, f: Tree)(@unused ev: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix}.lift[$F].withFieldComputedF($selector, $f)"
//   }

//   def withFieldRenamedImpl[C: WeakTypeTag](selectorFrom: Tree, selectorTo: Tree): Tree = {
//     val (fieldNameFrom, fieldNameTo) = (selectorFrom, selectorTo).extractSelectorsOrAbort
//     c.prefix.tree.renameField[C](fieldNameFrom, fieldNameTo)
//   }

//   def withCoproductInstanceImpl[To: WeakTypeTag, Inst: WeakTypeTag, C: WeakTypeTag](f: Tree): Tree = {
//     c.prefix.tree.overrideCoproductInstance[C](weakTypeOf[Inst], weakTypeOf[To], f, coproductInstanceT)
//   }

//   def withCoproductInstanceFImpl[F[+_]](f: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
//     q"${c.prefix}.lift[$F].withCoproductInstanceF($f)"
//   }

// }

import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.dsl.TransformerInto
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.TransformerFlags
import io.scalaland.chimney.internal.utils.DslMacroUtils

import scala.quoted._

object TransformerDefinitionWhiteboxMacros {
  def withFieldConstImpl[C: Type, From: Type, To: Type, T: Type, U: Type, Flags <: TransformerFlags: Type](using Quotes)(
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flags]], selector: Expr[To => T], value: Expr[U]
    ) = {
    new TransformerDefinitionWhiteboxMacros(quotes).withFieldConstImpl[C, From, To, T, U, Flags](td, selector, value)
  }

  def withFieldRenamedImpl[C: Type, From: Type, To: Type, T: Type, U: Type, Flags <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flags]], selectorFrom: Expr[From => T], selectorTo: Expr[To => U]
    ) = {
      new TransformerDefinitionWhiteboxMacros(quotes).withFieldRenamedImpl[C, From, To, T, U, Flags](td, selectorFrom, selectorTo)
  }

  def withCoproductInstanceImpl[C: Type, From: Type, To: Type, Inst: Type, Flags <: io.scalaland.chimney.internal.TransformerFlags: Type](using Quotes)(
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flags]], f: Expr[Inst => To]) = {
      new TransformerDefinitionWhiteboxMacros(quotes).withCoproductInstanceImpl[C, From, To, Inst, Flags](td, f)
  }
}

class TransformerDefinitionWhiteboxMacros(val topLevelQuotes: Quotes) extends DslMacroUtils {

  import CfgTpes._

  def withFieldConstImpl[C: Type, From: Type, To: Type, T: Type, U: Type, Flags <: io.scalaland.chimney.internal.TransformerFlags: Type](
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flags]], selector: Expr[To => T], value: Expr[U]
    ) = {
    td.overrideField[C](extractSelectorFieldName(selector), value, fieldConstT)
  }

  def withFieldComputedImpl[C: Type, From: Type, To: Type, T: Type, U: Type, Flags <: io.scalaland.chimney.internal.TransformerFlags: Type](
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flags]], selector: Expr[To => T], f: Expr[From => U]
    ) = {
    td.overrideField[C](extractSelectorFieldName(selector), f, fieldComputedT)
  }

  def withFieldRenamedImpl[C: Type, From: Type, To: Type, T: Type, U: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flagss]], selectorFrom: Expr[From => T], selectorTo: Expr[To => U]
    ) = {
    val (fieldNameFrom, fieldNameTo) = (selectorFrom, selectorTo).extractSelectorsOrAbort
    td.renameField[C](fieldNameFrom, fieldNameTo)
  }

  def withCoproductInstanceImpl[C: Type, From: Type, To: Type, Inst: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type](
    td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flagss]], f: Expr[Inst => To]) = {
    import topLevelQuotes.reflect._
    td.overrideCoproductInstance[C, Inst](f, coproductInstanceT)
  }
}