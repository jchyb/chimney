package io.scalaland.chimney.dsl

import io.scalaland.chimney.Iso
import io.scalaland.chimney.internal.runtime.{TransformerFlags, TransformerOverrides}

final class IsoDefinition[
    From,
    To,
    FromOverrides <: TransformerOverrides,
    ToOverrides <: TransformerOverrides,
    Flags <: TransformerFlags
](
    val from: TransformerDefinition[From, To, FromOverrides, Flags],
    val to: TransformerDefinition[To, From, ToOverrides, Flags]
) extends TransformerFlagsDsl[Lambda[
      `Flags1 <: TransformerFlags` => CodecDefinition[From, To, FromOverrides, ToOverrides, Flags1]
    ], Flags] {

  // TODO: def withFieldRenamed

  def buildCodec[ImplicitScopeFlags <: TransformerFlags](implicit
      tc: TransformerConfiguration[ImplicitScopeFlags]
  ): Iso[From, To] = ???
  // macro TransformerMacros.deriveTotalTransformerWithConfig[From, To, Overrides, Flags, ImplicitScopeFlags]
}