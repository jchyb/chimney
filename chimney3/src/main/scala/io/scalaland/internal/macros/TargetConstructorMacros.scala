package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.dsl.TransformerDefinition

import scala.quoted._
import scala.annotation.experimental

trait TargetConstructorMacros extends Model {

  @experimental
  def mkNewClass[ClassType: Type](using Quotes)(args: Iterable[Expr[Any]]): Expr[ClassType] = {
    import quotes.reflect._
    val tpeArgs = TypeRepr.of[ClassType].typeArgs
    {
      if tpeArgs.isEmpty then
        Apply(Select.unique(New(TypeTree.of[ClassType]), "<init>"), args.map(_.asTerm).toList)
      else
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[ClassType]), "<init>"), tpeArgs.map(tpe => TypeIdent(tpe.typeSymbol))
          ),
          args.map(_.asTerm).toList
        )
    }.asExprOf[ClassType]
  }

  def mkNewTupleClass[TupleType: Type](using Quotes)(args: Iterable[Expr[Any]]): Expr[TupleType] = {
    import quotes.reflect._
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[TupleType]), "<init>"), args.map(_.asTerm.tpe.asType match {case '[t] => TypeTree.of[t]}).toList
      ),
      args.map(_.asTerm).toList
    ).asExprOf[TupleType]
  }

  // def mkNewJavaBean(classTpe: Type, argsMapping: Iterable[(Target, Tree)]): Tree = {

  //   val fn = freshTermName(classTpe)

  //   val objCreation = q"val $fn = new $classTpe"
  //   val setterInvocations = argsMapping.map {
  //     case (target, argTree) =>
  //       val setterName = TermName("set" + target.name.capitalize)
  //       q"$fn.$setterName($argTree)"
  //   }.toSeq

  //   q"{..${objCreation +: setterInvocations}; $fn}"
  // }

  def mkCoproductInstance[Src: Type, From: Type, To: Type](using Quotes)(
      transformerDefinitionPrefix: Expr[TransformerDefinition[_, _, _, _]],
      srcPrefixTree: Expr[Src => From],
      instSymbol: quotes.reflect.Symbol,
      derivationTarget: DerivationTarget
  ): Expr[Any] = {
    import quotes.reflect._
    val instFullName = Expr(instSymbol.fullName)
    val fullTargetName = Expr(TypeRepr.of[To].typeSymbol.fullName)
    val finalTpe = derivationTarget.targetType(TypeRepr.of[To])
    
    finalTpe.asType match {
      case '[ft] =>
        '{
          (t: Src) =>
            $transformerDefinitionPrefix
            .instances(($instFullName, $fullTargetName))
            .asInstanceOf[From => ft]
            .apply($srcPrefixTree(t))
        }
    }
  }

  def mkTransformerBodyTree0[T](derivationTarget: DerivationTarget)(targetValueTree: Expr[T]): Expr[T] = {
    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        targetValueTree
      // case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
      //   q"${wrapperSupportInstance}.pure($targetValueTree)"
    }
  }

  def mkTransformerBodyTree1[T: Type, To: Type](using Quotes)(
      target: Target, // TODO: not used for now, contents may be incorrect
      transformerBodyTree: TransformerBodyTree,
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Expr[T => Any] => Expr[T => To]
  ): Expr[T => To] = {
    mkTransformerBodyTree[T, To](Seq(target), Seq(transformerBodyTree), derivationTarget) {
      case Seq(innerTree) => mkTargetValueTree(innerTree)
    }
  }

  def mkTransformerBodyTree[T: Type, To: Type](using Quotes)(
      targets: Seq[Target], // TODO: not used for now, contents may be incorrect
      bodyTreeArgs: Seq[TransformerBodyTree],
      derivationTarget: DerivationTarget
  )(
      mkTargetValueTree: Seq[Expr[T => Any]] => Expr[T => To]
  ): Expr[T => To] = {
    import quotes.reflect._

    derivationTarget match {
      case DerivationTarget.TotalTransformer =>
        mkTargetValueTree(bodyTreeArgs.map(_.tree.asExprOf[T => Any]))
      // case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
      //   val (pureArgs, wrappedArgs) = (targets zip bodyTreeArgs).partition(_._2.isTotalTarget)

      //   if (wrappedArgs.isEmpty) {
      //     q"$wrapperSupportInstance.pure(${mkTargetValueTree(bodyTreeArgs.map(_.tree))})"
      //   } else {

      //     val (wrappedTargets, wrappedBodyTrees) = wrappedArgs.unzip
      //     val wrappedTrees = wrappedBodyTrees.map(_.tree)
      //     val productF = wrappedTrees.reduceRight { (tree, rest) =>
      //       q"$wrapperSupportInstance.product($tree, $rest)"
      //     }

      //     val argNames = wrappedTargets.map(target => freshTermName(target.name))
      //     val argTypes = wrappedTargets.map(_.tpe)
      //     val bindTreesF = argNames.map { termName =>
      //       Bind(termName, Ident(termNames.WILDCARD))
      //     }
      //     val productType = argTypes.map(tpe => tq"$tpe").reduceRight[Tree]((param, tree) => tq"($param, $tree)")
      //     val patternF = bindTreesF.reduceRight[Tree]((param, tree) => pq"(..${List(param, tree)})")

      //     val patRefArgsMap = (wrappedTargets zip argNames).map { case (target, argName) => target -> q"$argName" }.toMap
      //     val pureArgsMap = pureArgs.map { case (target, bt)                             => target -> bt.tree }.toMap
      //     val argsMap = pureArgsMap ++ patRefArgsMap

      //     val updatedArgs = targets.map(argsMap)

      //     q"""
      //       $wrapperSupportInstance.map[$productType, $To](
      //         $productF,
      //         { case $patternF => ${mkTargetValueTree(updatedArgs)} }
      //       )
      //     """
      //   }
    }
  }
}
