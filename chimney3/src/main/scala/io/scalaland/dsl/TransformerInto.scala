package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerCfg._
import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.macros.dsl.{TransformerBlackboxMacros, TransformerIntoWhiteboxMacros}

import scala.language.experimental.macros

object TransformerInto {
  extension [From, To, C <: TransformerCfg, Flags <: TransformerFlags] (inline transformerInto: TransformerInto[From, To, C, Flags]) {
    /** Use `value` provided here for field picked using `selector`.
      *
      * By default if `From` is missing field picked by `selector` compilation fails.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      */
    transparent inline def withFieldConst[T, U](inline selector: To => T, inline value: U)(
      implicit inline ev: U <:< T
    ): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
      ${ TransformerIntoWhiteboxMacros.withFieldConstImpl[From, To, C, T, U, Flags]('transformerInto, 'selector, 'value, 'ev) }
    
    /** Use `map` provided here to compute value of field picked using `selector`.
      *
      * By default if `From` is missing field picked by `selector` compilation fails.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#providing-missing-values]] for more details
      * @param selector target field in `To`, defined like `_.name`
      * @param f        function used to compute value of the target field
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      * */
    transparent inline def withFieldComputed[T, U](
        inline selector: To => T,
        inline f: From => U
    )(implicit inline ev: U <:< T): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
      ${ TransformerIntoWhiteboxMacros.withFieldComputedImpl[From, To, C, T, U, Flags]('transformerInto, 'selector, 'f, 'ev) }
  
    /** Use `selectorFrom` field in `From` to obtain the value of `selectorTo` field in `To`
      *
      * By default if `From` is missing field picked by `selectorTo` compilation fails.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#fields-renaming]] for more details
      * @param selectorFrom source field in `From`, defined like `_.originalName`
      * @param selectorTo   target field in `To`, defined like `_.newName`
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      * */
    transparent inline def withFieldRenamed[T, U](
      inline selectorFrom: From => T,
      inline selectorTo: To => U
    ): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
      ${TransformerIntoWhiteboxMacros.withFieldRenamedImpl[From, To, C, T, U, Flags]('transformerInto, 'selectorFrom, 'selectorTo)}
    
    /** Use `f` to calculate the (missing) coproduct instance when mapping one coproduct into another
      *
      * By default if mapping one coproduct in `From` into another coproduct in `To` derivation
      * expects that coproducts will have matching names of its components, and for every component
      * in `To` field's type there is matching component in `From` type. If some component is missing
      * it will fail.
      *
      * @see [[https://scalalandio.github.io/chimney/transformers/customizing-transformers.html#transforming-coproducts]] for more details
      * @param f function to calculate values of components that cannot be mapped automatically
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      */
    transparent inline def withCoproductInstance[Inst](f: Inst => To): TransformerInto[From, To, _ <: TransformerCfg, Flags] =
      ${TransformerIntoWhiteboxMacros.withCoproductInstanceImpl[From, Inst, To, C, Flags]('transformerInto, 'f)}

    /** Apply configured transformation in-place.
      *
      * It runs macro that tries to derive instance of `Transformer[From, To]`
      * and immediately apply it to captured `source` value.
      * When transformation can't be derived, it results with compilation error.
      *
      * @return transformed value of type `To`
      */
    inline def transform[ScopeFlags <: TransformerFlags](
        implicit inline tc: io.scalaland.chimney.dsl.TransformerConfiguration[ScopeFlags]
    ): To = ${ TransformerBlackboxMacros.transformImpl[From, To, C, Flags, ScopeFlags]('transformerInto, 'tc) }
  }
}

/** Provides DSL for configuring [[io.scalaland.chimney.Transformer]]'s
  * generation and using the result to transform value at the same time
  *
  * @param  source object to transform
  * @param  td     transformer definition
  * @tparam From   type of input value
  * @tparam To     type of output value
  * @tparam C      type-level encoded config
  */
final class TransformerInto[From, To, C <: TransformerCfg, Flags <: TransformerFlags](
    val source: From,
    val td: TransformerDefinition[From, To, C, Flags]
) extends FlagsDsl[({type MyLambda[F1 <: TransformerFlags] = TransformerInto[From, To, C, F1]})#MyLambda, Flags] {

  /** Used internally by macro. Please don't use in your code.
    */
  transparent inline def __refineTransformerDefinition[C1 <: TransformerCfg](
      f: TransformerDefinition[From, To, C, Flags] => TransformerDefinition[From, To, C1, Flags]
  ): TransformerInto[From, To, C1, Flags] =
    new TransformerInto[From, To, C1, Flags](source, f(td))

}
