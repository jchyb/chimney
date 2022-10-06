package io.scalaland.chimney.dsl

import io.scalaland.chimney.internal.TransformerFlags

type MyLambda[F1 <: TransformerFlags] = TransformerConfiguration[F1]

class TransformerConfiguration[Flags <: TransformerFlags]
    extends FlagsDsl[MyLambda, Flags]

object TransformerConfiguration {

  implicit val default: TransformerConfiguration[TransformerFlags.Default] =
    new TransformerConfiguration[TransformerFlags.Default]
}
