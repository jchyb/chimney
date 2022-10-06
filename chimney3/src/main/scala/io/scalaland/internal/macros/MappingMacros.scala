package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{TypeTestUtils, DslMacroUtils}
import io.scalaland.chimney.internal.{TransformerDerivationError, IncompatibleSourceTuple}

import scala.collection.immutable.ListMap
import scala.quoted._
import scala.annotation.experimental
trait MappingMacros extends Model with TypeTestUtils with DslMacroUtils {

  val topLevelQuotes: Quotes

  @experimental
  def resolveSourceTupleAccessors[From: Type, To: Type]: 
    Either[Seq[TransformerDerivationError], Map[Target, AccessorResolution.Resolved]] = {
    import topLevelQuotes.reflect._
    val tupleElems = TypeRepr.of[From].caseClassParams
    val targetFields = TypeRepr.of[To].caseClassParams

    if (tupleElems.size != targetFields.size) {
      Left {
        Seq(
          IncompatibleSourceTuple(
            tupleElems.size,
            targetFields.size,
            TypeRepr.of[From].typeSymbol.fullName,
            TypeRepr.of[To].typeSymbol.fullName
          )
        )
      }
    } else {
      Right {
        (tupleElems zip targetFields).map {
          case (tupleElem, targetField) =>
            Target.fromField[To](targetField) -> AccessorResolution.Resolved(tupleElem, wasRenamed = false)
        }.toMap
      }
    }
  }

  def resolveAccessorsMapping[From: Type](
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, AccessorResolution] = {
    import topLevelQuotes.reflect._
    val fromGetters = TypeRepr.of[From].getterMethods
    val accessorsMapping = targets
      .map { target =>
        target -> {
          val lookupName = config.fieldOverrides.get(target.name) match {
            case Some(FieldOverride.RenamedFrom(sourceName)) => sourceName
            case _                                           => target.name
          }
          val wasRenamed = lookupName != target.name
          fromGetters
            .map(lookupAccessor[From](config, lookupName, wasRenamed))
            .find(_ != AccessorResolution.NotFound)
            .getOrElse(AccessorResolution.NotFound)
        }
      }

    ListMap(accessorsMapping.toSeq: _*)
  }

  def resolveOverrides[Src: Type, From: Type](
      srcPrefixTree: Expr[Src => From],
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, TransformerBodyTree] = {
    targets.flatMap { target =>
      target.tpe match 
        case '[t] =>
          config.fieldOverrides.get(target.name) match {
            case Some(FieldOverride.Const) =>
              Some {
                target -> TransformerBodyTree(
                  config.transformerDefinitionPrefix.get.accessOverriddenConstValue[t](target.name),
                  DerivationTarget.TotalTransformer
                )
              }
            // case Some(FieldOverride.ConstF) if config.derivationTarget.isInstanceOf[DerivationTarget.LiftedTransformer] =>
            //   val fTargetTpe = config.derivationTarget.targetType(target.tpe)
            //   Some {
            //     target -> TransformerBodyTree(
            //       config.transformerDefinitionPrefix.accessOverriddenConstValue(target.name, fTargetTpe),
            //       config.derivationTarget
            //     )
            //   }
            case Some(FieldOverride.Computed) =>
              Some {
                target -> TransformerBodyTree(
                  callUnaryApply[Src, From, t](
                    config.transformerDefinitionPrefix.get.accessOverriddenComputedFunction[From, t](target.name),
                    srcPrefixTree
                  ),
                  DerivationTarget.TotalTransformer
                )
              }
            // case Some(FieldOverride.ComputedF)
            //     if config.derivationTarget.isInstanceOf[DerivationTarget.LiftedTransformer] =>
            //   val fTargetTpe = config.derivationTarget.targetType(target.tpe)
            //   Some {
            //     target -> TransformerBodyTree(
            //       config.transformerDefinitionPrefix
            //         .accessOverriddenComputedFunction(target.name, From, fTargetTpe)
            //         .callUnaryApply(srcPrefixTree),
            //       config.derivationTarget
            //     )
            //   }
            case _ =>
              None
      }
    }.toMap
  }

  def resolveFallbackTransformerBodies[To: Type](
      targets: Iterable[Target],
      config: TransformerConfig
  ): Map[Target, TransformerBodyTree] = {
    import topLevelQuotes.reflect._

    lazy val targetCaseClassDefaults = TypeRepr.of[To].typeSymbol.caseClassDefaults

    val fallbackTransformers = targets.flatMap { target =>
      def defaultValueFallback =
        if (config.flags.processDefaultValues && TypeRepr.of[To].isCaseClass) {
          given Quotes = topLevelQuotes
          targetCaseClassDefaults
            .get(target.name)
            .map(defaultValueTree => target -> TransformerBodyTree('{(_: Any) => $defaultValueTree}, DerivationTarget.TotalTransformer))
        } else {
          None
        }

      def optionNoneFallback = {
        given Quotes = topLevelQuotes
        target.tpe match 
          case '[t] =>
            if (config.flags.optionDefaultsToNone && isOption[t]) {
              Some(target -> TransformerBodyTree('{(_: Any) => None}, DerivationTarget.TotalTransformer))
            } else {
              None
            }
      }

      def unitFallback = {
        given Quotes = topLevelQuotes
        target.tpe match
          case '[t] =>
            if (isUnit[t]) {
              Some(target -> TransformerBodyTree('{(_: Any) => ()}, DerivationTarget.TotalTransformer))
            } else {
              None
            }
      }

      defaultValueFallback orElse optionNoneFallback orElse unitFallback
    }
    ListMap(fallbackTransformers.toSeq: _*)
  }

  def lookupAccessor[From: Type](
      config: TransformerConfig,
      lookupName: String,
      wasRenamed: Boolean
  )(ms: topLevelQuotes.reflect.Symbol): AccessorResolution = {
    import topLevelQuotes.reflect._
    val sourceName = ms.name.toString
    // if (config.flags.beanGetters) {
    //   val lookupNameCapitalized = lookupName.capitalize
    //   if (sourceName == lookupName ||
    //       sourceName == s"get$lookupNameCapitalized" ||
    //       (sourceName == s"is$lookupNameCapitalized" && ms.resultTypeIn(From) == typeOf[Boolean])) {
    //     AccessorResolution.Resolved(ms, wasRenamed = false)
    //   } else {
    //     AccessorResolution.NotFound
    //   }
    // } else {
    if (sourceName == lookupName) {
      if (ms.isValDef || wasRenamed || config.flags.methodAccessors) { // TODO isStable changed to isValDef - unsure if correct
        AccessorResolution.Resolved(ms, wasRenamed)
      } else {
        AccessorResolution.DefAvailable
      }
    } else {
      AccessorResolution.NotFound
    }
    // }
  }

}
