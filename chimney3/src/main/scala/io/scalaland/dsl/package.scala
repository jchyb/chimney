package io.scalaland.chimney

import io.scalaland.chimney.internal.{TransformerCfg, TransformerFlags}

/** Main object to import in order to use Chimney's features
  */
package object dsl {

  /** Provides transformer operations on values of any type.
    *
    * @param source wrapped source value
    * @tparam From type of source value
    */
  implicit class TransformerOps[From](private val source: From) extends AnyVal {

    /** Allows to customize transformer generation to your target type.
      *
      * @tparam To target type
      * @return [[io.scalaland.chimney.dsl.TransformerInto]]
      */
    final def into[To]: TransformerInto[From, To, TransformerCfg.Empty, TransformerFlags.Default] =
      new TransformerInto(source, new TransformerDefinition(Map.empty, Map.empty))

    /** Performs in-place transformation of captured source value to target type.
      *
      * If you want to customize transformer behavior, consider using
      * [[io.scalaland.chimney.dsl.TransformerOps#into]] method.
      *
      * @see [[io.scalaland.chimney.Transformer#derive]] for default implicit instance
      * @param transformer implicit instance of [[io.scalaland.chimney.Transformer]] type class
      * @tparam To target type
      * @return transformed value of target type `To`
      */
    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }
}
