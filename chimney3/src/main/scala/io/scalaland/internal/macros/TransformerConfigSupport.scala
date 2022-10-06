package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.TransformerFlags
import io.scalaland.chimney.internal.utils.MacroUtils
import io.scalaland.chimney.dsl.TransformerDefinition

import scala.quoted._
import scala.annotation.experimental

trait TransformerConfigSupport extends MacroUtils {

  val topLevelQuotes: Quotes
  import topLevelQuotes.reflect._

  @experimental
  def readConfig[C: Type, InstanceFlags: Type, ScopeFlags: Type](using Quotes): TransformerConfig = {
    val scopeFlags = captureTransformerFlags[ScopeFlags]()
    val combinedFlags = captureTransformerFlags[InstanceFlags](scopeFlags)

    captureTransformerConfig[C].copy(flags = combinedFlags)
  }

  sealed abstract class FieldOverride(val needValueLevelAccess: Boolean)

  object FieldOverride {
    case object Const extends FieldOverride(true)
    // case object ConstF extends FieldOverride(true)
    case object Computed extends FieldOverride(true)
    // case object ComputedF extends FieldOverride(true)
    case class RenamedFrom(sourceName: String) extends FieldOverride(false)
  }

  import scala.quoted._
  sealed trait DerivationTarget {
    def targetType(using Quotes)(toTpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr
  }
  object DerivationTarget {
    // derivation target instance of `Transformer[A, B]`
    case object TotalTransformer extends DerivationTarget {
      def targetType(using Quotes)(toTpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = toTpe

    }
    // derivation target instace of `TransformerF[F, A, B]`, where F is wrapper type
    // case class LiftedTransformer(
    //     wrapperType: TypeRepr,
    //     wrapperSupportInstance: Tree = EmptyTree,
    //     wrapperErrorPathSupportInstance: Option[Tree] = None
    // ) extends DerivationTarget {
    //   def targetType(toTpe: Type): Type = wrapperType.applyTypeArg(toTpe)
    // }
  }

  case class TransformerConfig(
      derivationTarget: DerivationTarget = DerivationTarget.TotalTransformer,
      flags: TransformerFlags = TransformerFlags(),
      fieldOverrides: Map[String, FieldOverride] = Map.empty,
      coproductInstances: Set[(Symbol, TypeRepr)] = Set.empty, // pair: inst type, target type
      transformerDefinitionPrefix: Option[Expr[TransformerDefinition[_, _, _, _]]] = None, // EmptyTree,
      definitionScope: Option[(Symbol, Symbol)] = None,
      // coproductInstancesF: Set[(Symbol, Type)] = Set.empty // pair: inst type, target type
  ) {

    def withDerivationTarget(derivationTarget: DerivationTarget): TransformerConfig = {
      copy(derivationTarget = derivationTarget)
    }

    def withTransformerDefinitionPrefix(tdPrefix: Expr[TransformerDefinition[_, _, _, _]]): TransformerConfig =
      copy(transformerDefinitionPrefix = Some(tdPrefix))
  
    def withDefinitionScope[From: Type, To: Type](using Quotes): TransformerConfig = {
      val fromTpe = TypeRepr.of[From].typeSymbol
      val toTpe = TypeRepr.of[To].typeSymbol
      copy(definitionScope = Some((fromTpe, toTpe)))
    }

    def rec: TransformerConfig =
      copy(
        definitionScope = None,
        fieldOverrides = Map.empty
      )

    def valueLevelAccessNeeded: Boolean = {
      fieldOverrides.exists { case (_, fo) => fo.needValueLevelAccess } ||
      coproductInstances.nonEmpty 
      // || coproductInstancesF.nonEmpty
    }

    def fieldOverride(fieldName: String, fieldOverride: FieldOverride): TransformerConfig = {
      copy(fieldOverrides = fieldOverrides + (fieldName -> fieldOverride))
    }

    def coproductInstance(instanceType: TypeRepr, targetType: TypeRepr): TransformerConfig = {
      copy(coproductInstances = coproductInstances + (instanceType.typeSymbol -> targetType))
    }

    // def coproductInstanceF(instanceType: Type, targetType: Type): TransformerConfig = {
    //   copy(coproductInstancesF = coproductInstancesF + (instanceType.typeSymbol -> targetType))
    // }
  }

  object CfgTpes {
    import scala.quoted._
    import io.scalaland.chimney.internal.TransformerCfg._

    // We can check if the type of HKT is equal by comparing symbols

    val emptyT = topLevelQuotes.reflect.TypeRepr.of[Empty]
    val fieldConstT = topLevelQuotes.reflect.TypeRepr.of[FieldConst]
    // val fieldConstFT: Type = typeOf[FieldConstF[_]].typeConstructor
    val fieldComputedT = topLevelQuotes.reflect.TypeRepr.of[FieldComputed]
    // val fieldComputedFT: Type = typeOf[FieldComputedF[_, _]].typeConstructor
    val fieldRelabelledT = topLevelQuotes.reflect.TypeRepr.of[FieldRelabelled]
    val coproductInstanceT = topLevelQuotes.reflect.TypeRepr.of[CoproductInstance]
    // val coproductInstanceFT: Type = typeOf[CoproductInstanceF[_, _, _]].typeConstructor
    // val wrapperTypeT: Type = typeOf[WrapperType[F, _] forSome { type F[+_] }].typeConstructor
  }

  // def extractWrapperType(rawCfgTpe: Type): Type = {
  //   import CfgTpes._
  //   val cfgTpe = rawCfgTpe.dealias
  //   if (cfgTpe =:= emptyT) {
  //     // $COVERAGE-OFF$
  //     c.abort(c.enclosingPosition, "Expected WrapperType passed to transformer configuration!")
  //     // $COVERAGE-ON$
  //   } else if (cfgTpe.typeConstructor =:= wrapperTypeT) {
  //     val List(f, _) = cfgTpe.typeArgs
  //     f
  //   } else if (cfgTpe.typeArgs.nonEmpty) {
  //     extractWrapperType(cfgTpe.typeArgs.last)
  //   } else {
  //     // $COVERAGE-OFF$
  //     c.abort(c.enclosingPosition, "Bad internal transformer config type shape!")
  //     // $COVERAGE-ON$
  //   }
  // }

  @experimental
  def captureTransformerConfig[T: Type]: TransformerConfig = {
    import topLevelQuotes.reflect._
    import CfgTpes._

    val cfgTpe = TypeRepr.of[T].dealias
    given Quotes = topLevelQuotes

    if (cfgTpe.typeSymbol == emptyT.typeSymbol) {
      TransformerConfig()
    } else if (cfgTpe.typeSymbol == fieldConstT.typeSymbol) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      rest.asType match
        case '[t] => captureTransformerConfig[t].fieldOverride(fieldName, FieldOverride.Const)
    } else if (cfgTpe.typeSymbol == fieldComputedT.typeSymbol) {
      val List(fieldNameT, rest) = cfgTpe.typeArgs
      val fieldName = fieldNameT.singletonString
      rest.asType match
        case '[t] => captureTransformerConfig[t].fieldOverride(fieldName, FieldOverride.Computed)
    } else if (cfgTpe.typeSymbol == fieldRelabelledT.typeSymbol) {
      val List(fieldNameFromT, fieldNameToT, rest) = cfgTpe.typeArgs
      val fieldNameFrom = fieldNameFromT.singletonString
      val fieldNameTo = fieldNameToT.singletonString
      rest.asType match
        case '[t] =>
          captureTransformerConfig[t]
            .fieldOverride(fieldNameTo, FieldOverride.RenamedFrom(fieldNameFrom))
    } else if (cfgTpe.typeSymbol == coproductInstanceT.typeSymbol) {
      val List(instanceType, targetType, rest) = cfgTpe.typeArgs
      rest.asType match
        case '[t] =>
          captureTransformerConfig[t]
            .coproductInstance(instanceType, targetType)
    // } else if (cfgTpe.typeConstructor =:= wrapperTypeT) { // extracted already at higher level by extractWrapperType
    //   captureTransformerConfig(cfgTpe.typeArgs.last)
    // } else if (cfgTpe.typeConstructor =:= fieldConstFT) {
    //   val List(fieldNameT, rest) = cfgTpe.typeArgs
    //   val fieldName = fieldNameT.singletonString
    //   captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.ConstF)
    // } else if (cfgTpe.typeConstructor =:= fieldComputedFT) {
    //   val List(fieldNameT, rest) = cfgTpe.typeArgs
    //   val fieldName = fieldNameT.singletonString
    //   captureTransformerConfig(rest).fieldOverride(fieldName, FieldOverride.ComputedF)
    // } else if (cfgTpe.typeConstructor =:= coproductInstanceFT) {
    //   val List(instanceType, targetType, rest) = cfgTpe.typeArgs
    //   captureTransformerConfig(rest).coproductInstanceF(instanceType, targetType)
    } else {
      // $COVERAGE-OFF$
      report.errorAndAbort("Bad internal transformer config type shape!", Position.ofMacroExpansion)
      // $COVERAGE-ON$
    }
  }

  case class TransformerFlags(
      methodAccessors: Boolean = false,
      processDefaultValues: Boolean = true,
      beanSetters: Boolean = false,
      beanGetters: Boolean = false,
      optionDefaultsToNone: Boolean = false,
      unsafeOption: Boolean = false
  ) {
    def setFlag(using Quotes)(flagTpe: quotes.reflect.TypeRepr, value: Boolean): TransformerFlags = {
      if (flagTpe =:= FlagsTpes.methodAccessorsT) {
        copy(methodAccessors = value)
      } else if (flagTpe =:= FlagsTpes.defaultValuesT) {
        copy(processDefaultValues = value)
      } else if (flagTpe =:= FlagsTpes.beanSettersT) {
        copy(beanSetters = value)
      } else if (flagTpe =:= FlagsTpes.beanGettersT) {
        copy(beanGetters = value)
      } else if (flagTpe =:= FlagsTpes.optionDefaultsToNoneT) {
        copy(optionDefaultsToNone = value)
      } else if (flagTpe =:= FlagsTpes.unsafeOptionT) {
        copy(unsafeOption = value)
      } else {
        // $COVERAGE-OFF$
        import quotes.reflect._
        report.errorAndAbort(s"Invalid transformer flag type: $flagTpe!", Position.ofMacroExpansion)
        // $COVERAGE-ON$
      }
    }
  }

  object FlagsTpes {

    import io.scalaland.chimney.internal.TransformerFlags._

    def defaultT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Default]
    def enableT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Enable]
    def disableT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Disable]

    def methodAccessorsT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[MethodAccessors]
    def defaultValuesT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[DefaultValues]
    def beanSettersT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[BeanSetters]
    def beanGettersT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[BeanGetters]
    def optionDefaultsToNoneT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[OptionDefaultsToNone]
    def unsafeOptionT(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[UnsafeOption]
  }

  @experimental
  def captureTransformerFlags[T: Type](using Quotes)(
      defaultFlags: TransformerFlags = TransformerFlags()
  ): TransformerFlags = {
    import quotes.reflect._
    import FlagsTpes._

    val flagsTpe = TypeRepr.of[T].dealias

    if (flagsTpe.typeSymbol == defaultT.typeSymbol) {
      defaultFlags
    } else if (flagsTpe.typeSymbol == enableT.typeSymbol) {
      val List(flagT, rest) = flagsTpe.typeArgs
      rest.asType match
        case '[t] => captureTransformerFlags[t](defaultFlags).setFlag(flagT, value = true)
    } else if (flagsTpe.typeSymbol == disableT.typeSymbol) {
      val List(flagT, rest) = flagsTpe.typeArgs
      rest.asType match
        case '[t] => captureTransformerFlags[t](defaultFlags).setFlag(flagT, value = false)
    } else {
      // $COVERAGE-OFF$
      report.errorAndAbort("Bad internal transformer flags type shape!", Position.ofMacroExpansion)
      // $COVERAGE-ON$
    }
  }

  @experimental
  def captureFromTransformerConfigurationTree(using Quotes)(transformerConfigurationTree: Expr[Any]): TransformerFlags = {
    import quotes.reflect._
    transformerConfigurationTree.asTerm.tpe.widen.typeArgs.headOption
      .map(flagsTpe => 
        flagsTpe.asType match 
          case '[t] => captureTransformerFlags[t]())
      .getOrElse(TransformerFlags())
  }

}
