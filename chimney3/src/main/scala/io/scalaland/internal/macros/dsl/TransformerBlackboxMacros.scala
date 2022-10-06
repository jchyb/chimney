package io.scalaland.chimney.internal.macros.dsl

import scala.quoted._
import scala.annotation.unused
import scala.annotation.experimental

import io.scalaland.chimney.internal.macros.TransformerMacros
import io.scalaland.chimney.dsl.TransformerConfiguration
import io.scalaland.chimney.dsl.TransformerInto
import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg

object TransformerBlackboxMacros {
  @experimental
  def transformImpl[
    From: Type,
    To: Type,
    C <: io.scalaland.chimney.internal.TransformerCfg: Type,
    InstanceFlags <: io.scalaland.chimney.internal.TransformerFlags: Type,
    ScopeFlags <: io.scalaland.chimney.internal.TransformerFlags: Type
  ](using Quotes)(ti: Expr[TransformerInto[From, To, C, InstanceFlags]], @unused tc: Expr[TransformerConfiguration[ScopeFlags]]): Expr[To] = {
    new TransformerBlackboxMacros(quotes).transformImpl[From, To, C, InstanceFlags, ScopeFlags](ti, tc)
  }

  @experimental
  def deriveTransformerImpl[From: Type, To: Type](using Quotes)(): Expr[Transformer[From, To]] =
    new TransformerBlackboxMacros(quotes).deriveTransformerImpl[From, To]
  
  @experimental
  def buildTransformerImpl[
      From: Type,
      To: Type,
      C <: io.scalaland.chimney.internal.TransformerCfg: Type,
      Flags <: io.scalaland.chimney.internal.TransformerFlags: Type,
      ScopeFlags <: io.scalaland.chimney.internal.TransformerFlags: Type
  ](using Quotes)(td: Expr[TransformerDefinition[From, To, C, Flags]], @unused tc: Expr[TransformerConfiguration[ScopeFlags]]): Expr[Transformer[From, To]] =
    new TransformerBlackboxMacros(quotes).buildTransformerImpl[From, To, C, Flags, ScopeFlags](td, tc)
}

class TransformerBlackboxMacros(val topLevelQuotes: Quotes) extends TransformerMacros {

  @experimental
  def buildTransformerImpl[
      From: Type,
      To: Type,
      C <: TransformerCfg: Type,
      Flags <: io.scalaland.chimney.internal.TransformerFlags: Type,
      ScopeFlags <: io.scalaland.chimney.internal.TransformerFlags: Type
  ](using Quotes)(td: Expr[TransformerDefinition[From, To, C, Flags]], @unused tc: Expr[TransformerConfiguration[ScopeFlags]]): Expr[Transformer[From, To]] = {
      buildDefinedTransformer[From, To, C, Flags, ScopeFlags](td, DerivationTarget.TotalTransformer)
  }

//   def buildTransformerFImpl[
//       F[+_],
//       From: WeakTypeTag,
//       To: WeakTypeTag,
//       C: WeakTypeTag,
//       InstanceFlags: WeakTypeTag,
//       ScopeFlags: WeakTypeTag
//   ](
//       tfs: c.Expr[TransformerFSupport[F]],
//       @unused tc: c.Tree
//   ): c.Expr[TransformerF[F, From, To]] = {
//     val wrapperType = extractWrapperType(weakTypeOf[C])
//     val derivationTarget =
//       DerivationTarget.LiftedTransformer(wrapperType, tfs.tree, findTransformerErrorPathSupport(wrapperType))
//     c.Expr[TransformerF[F, From, To]](
//       buildDefinedTransformer[From, To, C, InstanceFlags, ScopeFlags](derivationTarget)
//     )
//   }

  @experimental
  def transformImpl[
      From: Type,
      To: Type,
      C <: io.scalaland.chimney.internal.TransformerCfg: Type ,
      InstanceFlags <: io.scalaland.chimney.internal.TransformerFlags: Type,
      ScopeFlags <: io.scalaland.chimney.internal.TransformerFlags: Type
  ](using Quotes)(ti: Expr[TransformerInto[From, To, C, InstanceFlags]], @unused tc: Expr[TransformerConfiguration[ScopeFlags]]): Expr[To] = {
    expandTransform[From, To, C, InstanceFlags, ScopeFlags](ti, DerivationTarget.TotalTransformer)
  }

//   def transformFImpl[
//       F[+_],
//       From: WeakTypeTag,
//       To: WeakTypeTag,
//       C: WeakTypeTag,
//       InstanceFlags: WeakTypeTag,
//       ScopeFlags: WeakTypeTag
//   ](
//       @unused tc: c.Tree,
//       tfs: c.Expr[TransformerFSupport[F]]
//   ): c.Expr[F[To]] = {
//     val wrapperType = extractWrapperType(weakTypeOf[C])
//     val derivationTarget =
//       DerivationTarget.LiftedTransformer(wrapperType, tfs.tree, findTransformerErrorPathSupport(wrapperType))
//     c.Expr[F[To]](expandTransform[From, To, C, InstanceFlags, ScopeFlags](derivationTarget))
//   }

  @experimental
  def deriveTransformerImpl[From: Type, To: Type](using Quotes): Expr[Transformer[From, To]] = {
    val tcTree = findLocalTransformerConfigurationFlags
    val flags = captureFromTransformerConfigurationTree(tcTree)

    val transformerTree = genTransformer[From, To](
      TransformerConfig(flags = flags).withDefinitionScope[From, To]
    )

    transformerTree
  }

//   def deriveTransformerFImpl[F[+_], From: WeakTypeTag, To: WeakTypeTag](
//       tfs: c.Expr[TransformerFSupport[F]]
//   )(implicit fwtt: WeakTypeTag[F[Nothing]]): c.Expr[TransformerF[F, From, To]] = {

//     val tcTree = findLocalTransformerConfigurationFlags
//     val flags = captureFromTransformerConfigurationTree(tcTree)
//     val wrapperType = fwtt.tpe.typeConstructor

//     val transformerTree = genTransformer[From, To](
//       TransformerConfig(
//         flags = flags,
//         definitionScope = Some((weakTypeOf[From], weakTypeOf[To]))
//       ).withDerivationTarget(
//         DerivationTarget.LiftedTransformer(
//           wrapperType = wrapperType,
//           wrapperSupportInstance = tfs.tree,
//           wrapperErrorPathSupportInstance = findTransformerErrorPathSupport(wrapperType)
//         )
//       )
//     )

//     c.Expr[chimney.TransformerF[F, From, To]] {
//       q"""{
//         val _ = $tcTree // hack to avoid unused warnings
//         $transformerTree
//       }"""
//     }
//   }
}
