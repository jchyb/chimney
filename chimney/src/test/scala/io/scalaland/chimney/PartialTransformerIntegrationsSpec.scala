package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.utils.OptionUtils.*

import scala.annotation.unused

class PartialTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*

  group("transform from Option-type into Option-type, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-empty") {
      val result = Possible(123).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible("123"))
      result.asEither ==> Right(Possible("123"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is empty") {
      val result = (Possible.Nope: Possible[Int]).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from OptionalValue into OptionalValue, using Partial Transformer for inner type transformation") {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.toPartialResultOrString("bad int"))

    test("when Result is success") {
      val result = Possible("123").transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Present(123))
      result.asEither ==> Right(Possible.Present(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = Possible("abc").transformIntoPartial[Possible[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = (Possible.Nope: Possible[String]).transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from non-OptionalValue into OptionalValue, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when inner value is non-null") {
      val result = 10.transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Present("10"))
      result.asEither ==> Right(Possible.Present("10"))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when inner value is null") {
      implicit val integerPrinter: Transformer[Integer, String] = _.toString

      val result = (null: Integer).transformIntoPartial[Possible[String]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group(
    "transform from non-OptionalValue into OptionalValue, using Partial Transformer for inner type transformation"
  ) {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.toPartialResultOrString("bad int"))

    test("when Result is success") {
      val result = "123".transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Present(123))
      result.asEither ==> Right(Possible.Present(123))
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when Result is failure") {
      val result = "abc".transformIntoPartial[Possible[Int]]
      result.asOption ==> None
      result.asEither ==> Left(
        partial.Result.Errors.fromString("bad int")
      )
      result.asErrorPathMessageStrings ==> Iterable(
        "" -> "bad int"
      )
    }

    test("when Result is null") {
      val result = (null: String).transformIntoPartial[Possible[Int]]

      result.asOption ==> Some(Possible.Nope)
      result.asEither ==> Right(Possible.Nope)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }
  }

  group("transform from OptionalValue into non-OptionalValue, using Total Transformer for inner type transformation") {

    implicit val intPrinter: Transformer[Int, String] = _.toString

    test("when option is non-empty") {
      val result = Possible(10).transformIntoPartial[String]

      result.asOption ==> Some("10")
      result.asEither ==> Right("10")
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is empty") {
      val result = Option.empty[Int].transformIntoPartial[String]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  group(
    "transform from OptionalValue into non-OptionalValue, using Partial Transformer for inner type transformation"
  ) {

    implicit val intPartialParser: PartialTransformer[String, Int] =
      PartialTransformer(_.parseInt.toPartialResultOrString("bad int"))

    test("when option is non-empty and inner is success") {
      val result = Possible("10").transformIntoPartial[Int]

      result.asOption ==> Some(10)
      result.asEither ==> Right(10)
      result.asErrorPathMessageStrings ==> Iterable.empty
    }

    test("when option is non-empty and inner is failure") {
      val result = Possible("abc").transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromErrorString("bad int"))
      result.asErrorPathMessageStrings ==> Iterable("" -> "bad int")
    }

    test("when option is empty") {
      val result = (Possible.Nope: Possible[String]).transformIntoPartial[Int]

      result.asOption ==> None
      result.asEither ==> Left(partial.Result.fromEmpty)
      result.asErrorPathMessageStrings ==> Iterable(("", "empty value"))
    }
  }

  // TODO: transform Iterable-type to Iterable-type, using Total Transformer for inner type transformation

  // TODO: transform Iterable-type to Iterable-type, using Partial Transformer for inner type transformation

  // TODO: transform between Array-type and Iterable-type, using Total Transformer for inner type transformation

  // TODO: transform between Array-type and Iterable-type, using Partial Transformer for inner type transformation

  // TODO: transform into sequential type with an override

  // TODO: transform Map-type to Map-type, using Total Transformer for inner type transformation

  // TODO: transform Map-type to Map-type, using Partial Transformer for inner type transformation

  // TODO: transform between Iterables and Maps, using Total Transformer for inner type transformation

  // TODO: transform between Iterables and Maps, using Partial Transformer for inner type transformation

  // TODO: transform between Arrays and Maps, using Total Transformer for inner type transformation

  // TODO: transform between Arrays and Maps, using Partial Transformer for inner type transformation

  // TODO: transform into map type with an override

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrorsFixed("""Source("foo").transformIntoPartial[TargetWithOption]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrorsFixed("""Source("foo").intoPartial[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo").intoPartial[TargetWithOption].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone
        Source("foo").transformIntoPartial[TargetWithOption].asOption ==> Some(TargetWithOption("foo", Possible.Nope))
      }
    }

    test(
      "should use OptionalValue.empty for fields without source but with default value when enabled but default values disabled"
    ) {
      Source("foo").intoPartial[TargetWithOptionAndDefault].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOptionAndDefault("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

        Source("foo").transformIntoPartial[TargetWithOptionAndDefault].asOption ==> Some(
          TargetWithOptionAndDefault("foo", Possible.Nope)
        )
      }
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .intoPartial[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOptionAndDefault(
          "foo",
          Possible.Present(42)
        )
      )
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrorsFixed("""Source("foo").intoPartial[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  group("flag .enablePartialUnwrapsOption") {

    case class Source(a: Possible[String])
    case class Target(a: String)

    test("should be turned on by default") {
      Source(Possible.Present("value")).transformIntoPartial[Target].asOption ==> Some(Target("value"))
      Source(Possible.Present("value")).intoPartial[Target].transform.asOption ==> Some(Target("value"))
    }

    test("should re-enable globally disabled .disablePartialUnwrapsOption") {
      implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

      Source(Possible.Present("value")).intoPartial[Target].enablePartialUnwrapsOption.transform.asOption ==> Some(
        Target("value")
      )
    }
  }

  group("flag .disablePartialUnwrapsOption") {

    @unused case class Source(a: Possible[String])
    @unused case class Target(a: String)

    test("should fail compilation if OptionalValue unwrapping is not provided when disabled") {
      compileErrorsFixed(
        """Source(Possible.Present("value")).intoPartial[Target].disablePartialUnwrapsOption.transform"""
      ).check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
        "java.lang.String",
        "derivation from source.a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to java.lang.String is not supported in Chimney!",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
        "a: java.lang.String - can't derive transformation from a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      locally {
        @unused implicit val config = TransformerConfiguration.default.disablePartialUnwrapsOption

        compileErrorsFixed("""Source(Possible.Present("value")).transformIntoPartial[Target]""").check(
          "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
          "java.lang.String",
          "derivation from source.a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] to java.lang.String is not supported in Chimney!",
          "io.scalaland.chimney.PartialTransformerIntegrationsSpec.Target",
          "a: java.lang.String - can't derive transformation from a: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[java.lang.String] in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
          "Consult https://chimney.readthedocs.io for usage examples."
        )
      }
    }
  }
}