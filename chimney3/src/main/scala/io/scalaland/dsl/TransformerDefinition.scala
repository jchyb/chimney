package io.scalaland.chimney.dsl

import io.scalaland.chimney.Transformer
import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerDefinitionWhiteboxMacros}

object TransformerDefinition {
  extension [From, To, C <: TransformerCfg, Flags <: TransformerFlags] (inline td: TransformerDefinition[From, To, C, Flags])
    /** Use `value` provided here for field picked using `selector`.
      *
      * By default if `From` is missing field picked by `selector` compilation fails.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
      * @param selector target field in `To`, defined like `_.name`
      * @param value    constant value to use for the target field
      * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
      */
    transparent inline def withFieldConst[T, U](inline selector: To => T, inline value: U)(
      implicit inline ev: U <:< T
    ) =
      ${ TransformerDefinitionWhiteboxMacros.withFieldConstImpl[C, From, To, T, U, Flags]('td, 'selector, 'value) }

    /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
      *
      * By default if `From` is missing field picked by `selectorTo` compilation fails.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
      * @param selectorFrom source field in `From`, defined like `_.originalName`
      * @param selectorTo   target field in `To`, defined like `_.newName`
      * @return [[io.scalaland.chimney.dsl.TransformerDefinition]]
      */
    transparent inline def withFieldRenamed[T, U](
        inline selectorFrom: From => T,
        inline selectorTo: To => U
    ): TransformerDefinition[From, To, _ <: TransformerCfg, Flags] =
      ${ TransformerDefinitionWhiteboxMacros.withFieldRenamedImpl[C, From, To, T, U, Flags]('td, 'selectorFrom, 'selectorTo) }
    
    /** Build Transformer using current configuration.
      *
      * It runs macro that tries to derive instance of `Transformer[From, To]`.
      * When transformation can't be derived, it results with compilation error.
      *
      * @return [[io.scalaland.chimney.Transformer]] type class instance
      */
    inline def buildTransformer[ScopeFlags <: TransformerFlags](
        implicit inline tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
    ): Transformer[From, To] =
      ${ TransformerBlackboxMacros.buildTransformerImpl[From, To, C, Flags, ScopeFlags]('td, 'tc) }
}


/** Allows customization of [[io.scalaland.chimney.Transformer]] derivation
  *
  * @tparam From type of input value
  * @tparam To   type of output value
  * @tparam C    type-level encoded config
  */
final class TransformerDefinition[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val overrides: Map[String, Any],
    val instances: Map[(String, String), Any]
) extends FlagsDsl[({type MyLambda[F1 <: TransformerFlags] = TransformerDefinition[From, To, C, F1]})#MyLambda, Flags]
    with TransformerDefinitionCommons[({type MyLambda[C1 <: TransformerCfg] = TransformerDefinition[From, To, C1, Flags]})#MyLambda] {

  override protected def updated(newOverrides: Map[String, Any], newInstances: Map[(String, String), Any]): this.type =
    new TransformerDefinition(newOverrides, newInstances).asInstanceOf[this.type]
}
