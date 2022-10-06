package io.scalaland.chimney.internal.utils

import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.TransformerConfigSupport

import scala.quoted._

trait DslMacroUtils extends TransformerConfigSupport with MacroUtils {

  val topLevelQuotes: Quotes
  import CfgTpes._

  extension (td: Expr[TransformerDefinition[_, _, _, _]]) {
    def accessOverriddenConstValue[T: Type](name: String): Expr[Any => T] = {
      '{ (_: Any) =>
        $td
          .overrides(${Expr(name)})
          .asInstanceOf[T]
      }
    }

    def accessOverriddenComputedFunction[From: Type, To: Type](name: String): Expr[From => To] = {
      '{
        $td
          .overrides(${Expr(name)})
          .asInstanceOf[From => To]
      }
    }
  }

  extension [From: Type, To: Type, Flagss <: io.scalaland.chimney.internal.TransformerFlags: Type] (td: Expr[TransformerDefinition[From, To, _ <: TransformerCfg, Flagss]]) {

    def overrideField[C: Type](
      fieldName: String, overrideTree: Expr[Any], configWrapperTC: topLevelQuotes.reflect.TypeRepr
      ) = {
        import topLevelQuotes.reflect._
        val singletonTpe = ConstantType(StringConstant(fieldName))
        val tpe = configWrapperTC.appliedTo(List(singletonTpe, TypeRepr.of[C]))
        
        tpe.asType match {
          case '[t] =>
            td.addOverride(fieldName, overrideTree).refineConfig[t].asExprOf[TransformerDefinition[From, To, _ <: TransformerCfg, Flagss]]
          }
    }

    def overrideCoproductInstance[C: Type, Inst: Type](
        f: Expr[Inst => To],
        configWrapperTC: topLevelQuotes.reflect.TypeRepr
      ) = {
      import topLevelQuotes.reflect._
      given Quotes = topLevelQuotes
      configWrapperTC.applyTypeArgs(TypeRepr.of[Inst], TypeRepr.of[To], TypeRepr.of[C]).asType match
        case '[t] =>
          td
            .addInstance(TypeRepr.of[Inst].typeSymbol.fullName, TypeRepr.of[To].typeSymbol.fullName, f)
            .refineConfig[t]
    }

    def renameField[C: Type](fromName: String, toName: String) = {
      import topLevelQuotes.reflect._
      given Quotes = topLevelQuotes
      fieldRelabelledT.applyTypeArgs(fromName.toSingletonTpe, toName.toSingletonTpe, TypeRepr.of[C]).asType match
        case '[t] => td.refineConfig[t].asExprOf[TransformerDefinition[From, To, _ <: TransformerCfg, Flagss]]
    }

    def addOverride(using Quotes)(fieldName: String, overrideTree: Expr[Any]) = {
      '{ $td.__addOverride(${Expr(fieldName)}, $overrideTree) }
    }

    def addInstance[Inst: Type](using Quotes)(fullInstName: String, fullTargetName: String, f: Expr[Inst => To]) = {
      '{$td.__addInstance(${Expr(fullInstName)}, ${Expr(fullTargetName)}, $f)}
    }

    def refineConfig[T: Type](using quotes: Quotes) = {
      import quotes.reflect._
      TypeApply(Select.unique(
        td.asTerm,"__refineConfig"),List(TypeTree.of[T]
      )).asExpr
    }
  }
}
