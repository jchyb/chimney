package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal._
import io.scalaland.chimney.internal.utils.EitherUtils
import io.scalaland.chimney.Transformer
import io.scalaland.chimney.dsl.TransformerInto
import io.scalaland.chimney.dsl.TransformerDefinition

import scala.quoted._
import scala.annotation.experimental

trait TransformerMacros extends MappingMacros with TargetConstructorMacros with EitherUtils {

  val topLevelQuotes: Quotes
  import topLevelQuotes.reflect._

  @experimental
  def buildDefinedTransformer[
      From: Type,
      To: Type,
      C <: TransformerCfg: Type,
      InstanceFlags <: io.scalaland.chimney.internal.TransformerFlags: Type,
      ScopeFlags <: io.scalaland.chimney.internal.TransformerFlags: Type
  ](td: Expr[TransformerDefinition[From, To, C, InstanceFlags]], derivationTarget: DerivationTarget): Expr[Transformer[From, To]] = {
    val config = readConfig[C, InstanceFlags, ScopeFlags]
      .withDefinitionScope[From, To]
      .withDerivationTarget(derivationTarget)

    if (!config.valueLevelAccessNeeded) {
      genTransformer[From, To](config)
    } else {
      val derivedTransformer = genTransformer[From, To](config.withTransformerDefinitionPrefix(td))

      derivedTransformer
    }
  }

  @experimental
  def expandTransform[
      From: Type,
      To: Type,
      C <: TransformerCfg: Type,
      InstanceFlags <: io.scalaland.chimney.internal.TransformerFlags: Type,
      ScopeFlags: Type
  ](ti: Expr[TransformerInto[From, To, C, InstanceFlags]], derivationTarget: DerivationTarget): Expr[To] = {
    val config = readConfig[C, InstanceFlags, ScopeFlags]
      .withTransformerDefinitionPrefix('{$ti.td})
      .withDerivationTarget(derivationTarget)
    
    val derivedTransformerTree = genTransformer[From, To](config)

    '{ ${derivedTransformerTree}.transform($ti.source) }
  }

  @experimental
  def genTransformer[From: Type, To: Type](
      config: TransformerConfig
  ): Expr[Transformer[From, To]] = {
    import quotes.reflect._

    genTransformerTree[From, To](config) match {

      case Right(transformerTree) =>
        config.derivationTarget match {
          // case DerivationTarget.LiftedTransformer(f, _, _) =>
          //   q"""
          //      new _root_.io.scalaland.chimney.TransformerF[$f, $From, $To] {
          //        def transform($srcName: $From): ${f.applyTypeArg(To)} = {
          //          $transformerTree
          //        }
          //      }
          //   """

          case DerivationTarget.TotalTransformer =>
            '{
              new _root_.io.scalaland.chimney.Transformer[From, To] {
                def transform(from: From): To = {
                  ${transformerTree}(from)
                }
              }
            }
        }

      case Left(derivationErrors) =>
        val fromStr = TypeRepr.of[From].show
        val toStr = TypeRepr.of[To].show
        val errorMessage =
          s"""Chimney can't derive transformation from $fromStr to $toStr
             |
             |${TransformerDerivationError.printErrors(derivationErrors)}
             |Consult $chimneyDocUrl for usage examples.
             |
             |""".stripMargin

        report.errorAndAbort(errorMessage, Position.ofMacroExpansion)
    }
  }

  @experimental
  def genTransformerTree[From: Type, To: Type](
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[From => To]] = {
    val srcPrefixTree = '{(f: From) => f}

    resolveTransformerBody[From, From, To](srcPrefixTree, config).map {
      case TransformerBodyTree(tree, derivedTarget) =>
        (config.derivationTarget, derivedTarget) match {
          // case (DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _), DerivationTarget.TotalTransformer) =>
          //   q"${wrapperSupportInstance}.pure[$To]($tree)"
          case _ =>
            tree.asExprOf[From => To]
        }
    }
  }

  @experimental
  def expandTransformerTree[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {

    resolveImplicitTransformer[From, To](config)
      .map(localImplicitTree => Right('{(t: Src) => ${localImplicitTree}.transform($srcPrefixTree(t)) }))
      .getOrElse {
        deriveTransformerTree[Src, From, To](srcPrefixTree, config)
      }
  }

  @experimental
  def deriveTransformerTree[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import quotes.reflect._
    if (isSubtype[From, To]) {
      expandSubtypes[Src, From, To](srcPrefixTree, config)
    } else if (fromValueClassToType[From, To]) {
      expandValueClassToType[Src, From, To](srcPrefixTree, config)
    } else if (fromTypeToValueClass[From, To]) {
      expandTypeToValueClass[Src, From, To](srcPrefixTree, config)
    } else if (bothOptions[From, To]) {
      expandOptions[Src, From, To](srcPrefixTree, config)
    } else if (isOption[To]) {
      expandTargetWrappedInOption[Src, From, To](srcPrefixTree, config)
    } else if (config.flags.unsafeOption && isOption[From]) {
      expandSourceWrappedInOption[Src, From, To](srcPrefixTree, config)
    } else if (bothEithers[From, To]) {
      expandEithers[Src, From, To](srcPrefixTree, config)
    } else if (isMap[From]) {
      expandFromMap[Src, From, To](srcPrefixTree, config)
    } else if (bothOfIterableOrArray[From, To]) {
      expandIterableOrArray[Src, From, To](srcPrefixTree, config)
    } else if (isTuple[To]) {
      expandDestinationTuple[Src, From, To](srcPrefixTree, config)
    } else if (destinationCaseClass[To]) {
      expandDestinationCaseClass[Src, From, To](srcPrefixTree, config)
    // } else if (config.flags.beanSetters && destinationJavaBean(To)) {
    //   expandDestinationJavaBean(srcPrefixTree, config)(From, To)
    } else if (bothSealedClasses[From, To]) {
      expandSealedClasses[Src, From, To](srcPrefixTree, config)
    } else {
      notSupportedDerivation[Src, From, To](srcPrefixTree)
    }

  }

  def expandSubtypes[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    Right {
      '{ (t: Src) => $srcPrefixTree(t).asInstanceOf[To] }
    }
  }

  @experimental
  def expandValueClassToType[Src: Type, From: Type, To: Type]                                                                                                             (
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {

    TypeRepr.of[From].valueClassMember
      .map { member =>
        Right {
          mkTransformerBodyTree0(config.derivationTarget) {
            '{(t: Src) => ${unsafeSelectByName[From, To]('{$srcPrefixTree(t)}, member.termSymbol.name)}}
          }
        }
      }
      .getOrElse {
        // $COVERAGE-OFF$
        Left {
          Seq(CantFindValueClassMember(TypeRepr.of[From].typeSymbol.name.toString, TypeRepr.of[To].typeSymbol.name.toString))
        }
        // $COVERAGE-ON$
      }
  }

  @experimental
  def expandTypeToValueClass[Src: Type, From: Type, To: Type](
     srcPrefixTree: Expr[Src => From],
     config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    Right {
      mkTransformerBodyTree0(config.derivationTarget) {
        '{ (t: Src) => ${ mkNewClass[To](List('{ ${srcPrefixTree.asExprOf[Src => Any]}(t) })) } }
      }
    }
  }

  @experimental
  def expandTargetWrappedInOption[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import quotes.reflect._

    if (TypeRepr.of[To] <:< noneTpe) {
      notSupportedDerivation[Src, From, To](srcPrefixTree)
    } else {
      given Type[Option[From]] = Type.of[Option[From]]
      expandOptions[Src, Option[From], To]('{(t: Src) => Option($srcPrefixTree(t))}, config)
    }
  }

  @experimental
  def expandSourceWrappedInOption[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import topLevelQuotes.reflect._
    given Quotes = topLevelQuotes
    if (TypeRepr.of[From] <:< noneTpe(using topLevelQuotes)) {
      notSupportedDerivation[Src, From, To](srcPrefixTree)
    } else {
      val fromInnerT = TypeRepr.of[From].typeArgs.head
      fromInnerT.asType match {
        case '[innerT] =>
          val innerSrcPrefix = '{ (t: Src) => $srcPrefixTree(t).asInstanceOf[Option[innerT]].get }
          resolveRecursiveTransformerBody[Src, innerT, To](innerSrcPrefix, config.rec)
            .map { innerTransformerBody =>
              // val fn = freshTermName(innerSrcPrefix).toString
              val fn = innerSrcPrefix.show // TODO, definitely incorrect, but not used for now
              mkTransformerBodyTree1[Src, To](Target(fn, Type.of[To](using topLevelQuotes).asInstanceOf[Type[Any]]), innerTransformerBody, config.derivationTarget) { tree =>
                tree.asExprOf[Src => To]
              }
            }
          }
    }
  }

  @experimental
  def expandOptions[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import topLevelQuotes.reflect._

    def fromInnerT = TypeRepr.of[From].collectionInnerTpe
    def toInnerT = TypeRepr.of[To].collectionInnerTpe

    if ((TypeRepr.of[From] <:< TypeRepr.of[Some[_]] && TypeRepr.of[To] <:< noneTpe(using topLevelQuotes)) || (TypeRepr.of[From] <:< noneTpe(using topLevelQuotes) && TypeRepr.of[To] <:< someTpe(using topLevelQuotes))) {
      notSupportedDerivation[Src, From, To](srcPrefixTree)
    } else {
      (fromInnerT.asType, toInnerT.asType) match
        case ('[from], '[to]) =>
          resolveRecursiveTransformerBody[from, from, to]('{t => t}, config.rec)
            .map {
              case TransformerBodyTree(innerTree, DerivationTarget.TotalTransformer) =>
                '{
                  t => ${srcPrefixTree.asExprOf[Src => Option[from]]}(t)
                  .map(fn => ${innerTree.asExprOf[from => to]}(fn)).asInstanceOf[To]
                }
              }
            //   case TransformerBodyTree(
            //       innerTree,
            //       DerivationTarget.LiftedTransformer(wrapperType, wrapperSupportInstance, _)
            //       ) =>
            //     q"""
            //       $srcPrefixTree.fold[${wrapperType.applyTypeArg(To)}](
            //         ${wrapperSupportInstance}.pure(Option.empty[$toInnerT])
            //       )(
            //         ($fn: $fromInnerT) => ${wrapperSupportInstance}.map($innerTree, Option.apply[$toInnerT])
            //       )
            //     """
            // }
    }
  }

  @experimental
  def expandEithers[Src: Type, From: Type, To: Type]( // TODO cleanup
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import topLevelQuotes.reflect._

    val List(fromLeftT, fromRightT) = TypeRepr.of[From].typeArgs
    val List(toLeftT, toRightT) = TypeRepr.of[To].typeArgs

    if (TypeRepr.of[From] <:< leftTpe(using topLevelQuotes) && !(TypeRepr.of[To] <:< rightTpe(using topLevelQuotes))) {
      given Quotes = topLevelQuotes
      (fromLeftT.asType, toLeftT.asType, fromRightT.asType, toRightT.asType) match
        case ('[fromLeft], '[toLeft], '[fromRight], '[toRight]) =>
          resolveRecursiveTransformerBody[Src, fromLeft, toLeft]('{(t: Src) => $srcPrefixTree(t).asInstanceOf[Left[fromLeft, fromRight]].value}, config.rec)
            .map { tbt =>
              mkTransformerBodyTree1[Src, To](Target("left", toLeftT.asType.asInstanceOf[Type[Any]]), tbt, config.derivationTarget) { leftArgTree =>
                '{ (t: Src) => new Left[toLeft, toRight](${leftArgTree.asExprOf[Src => toLeft]}(t)) }.asExprOf[Src => To]
              }
            }
    } else if (TypeRepr.of[From] <:< rightTpe(using topLevelQuotes) && !(TypeRepr.of[To] <:< leftTpe(using topLevelQuotes))) {
      given Quotes = topLevelQuotes
      (fromLeftT.asType, toLeftT.asType, fromRightT.asType, toRightT.asType) match
        case ('[fromLeft], '[toLeft], '[fromRight], '[toRight]) =>
          resolveRecursiveTransformerBody[Src, fromRight, toRight]('{(t: Src) => $srcPrefixTree(t).asInstanceOf[Right[fromLeft, fromRight]].value}, config.rec)
            .map { tbt =>
              mkTransformerBodyTree1[Src, To](Target("right", toRightT.asType.asInstanceOf[Type[Any]]), tbt, config.derivationTarget) {
                rightArgTree =>
                  '{ (t: Src) => new Right[toLeft, toRight](${rightArgTree.asExprOf[Src => toRight]}(t)) }.asExprOf[Src => To]
              }
            }
    } else if (!(TypeRepr.of[To] <:< leftTpe(using topLevelQuotes)) && !(TypeRepr.of[To] <:< rightTpe(using topLevelQuotes))) {
      given Quotes = topLevelQuotes
      val leftTransformerE = 
        (fromLeftT.asType, toLeftT.asType) match 
          case ('[fromLeft], '[toLeft]) => 
            resolveRecursiveTransformerBody[fromLeft, fromLeft, toLeft]('{t => t}, config.rec)
      val rightTransformerE = 
        (fromRightT.asType, toRightT.asType) match
          case ('[fromRight], '[toRight]) =>
            resolveRecursiveTransformerBody[fromRight, fromRight, toRight]('{t => t}, config.rec)

      (leftTransformerE, rightTransformerE) match {
        case (Right(leftTbt), Right(rightTbt)) =>

          (fromLeftT.asType, toLeftT.asType, fromRightT.asType, toRightT.asType) match
            case ('[fromLeft], '[toLeft], '[fromRight], '[toRight]) =>

              val leftBody = mkTransformerBodyTree1[fromLeft, To](Target("left", toLeftT.asType.asInstanceOf[Type[Any]]), leftTbt, config.derivationTarget) {
                leftArgTree =>
                  '{ (t: fromLeft) => new Left[toLeft, toRight](${leftArgTree.asExprOf[fromLeft => toLeft]}(t)) }.asExprOf[fromLeft => To]
              }

              val rightBody =
                mkTransformerBodyTree1[fromRight, To](Target("right", toRightT.asType.asInstanceOf[Type[Any]]), rightTbt, config.derivationTarget) {
                  rightArgTree =>
                    '{(t: fromRight) => new Right[toLeft, toRight](${rightArgTree.asExprOf[fromRight => toRight]}(t))}.asExprOf[fromRight => To]
                }

              Right {
                '{ (t: Src) => 
                  $srcPrefixTree(t).asInstanceOf[Either[fromLeft, fromRight]].fold[To](
                    (fnL: fromLeft) => $leftBody(fnL),
                    (fnR: fromRight) => $rightBody(fnR)
                  )
                }
              }
            case _ =>
              Left(leftTransformerE.left.getOrElse(Nil) ++ rightTransformerE.left.getOrElse(Nil))
      }
    } else {
      notSupportedDerivation[Src, From, To](srcPrefixTree)
    }
  }

  @experimental
  def expandFromMap[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    val ToInnerT = TypeRepr.of[To].collectionInnerTpe

    (config.derivationTarget, ToInnerT.caseClassParams.map(resultTypeIn(ToInnerT)(_))) match {
      // case (
      //     DerivationTarget.LiftedTransformer(
      //       wrapperType,
      //       wrapperSupportInstance,
      //       Some(wrapperErrorPathSupportInstance)
      //     ),
      //     List(toKeyT, toValueT)
      //     ) =>
      //   val List(fromKeyT, fromValueT) = From.typeArgs

      //   val fnK = Ident(freshTermName("k"))
      //   val fnV = Ident(freshTermName("v"))

      //   val keyTransformerE = resolveRecursiveTransformerBody(fnK, config.rec)(fromKeyT, toKeyT)
      //   val valueTransformerE = resolveRecursiveTransformerBody(fnV, config.rec)(fromValueT, toValueT)

      //   (keyTransformerE, valueTransformerE) match {
      //     case (Right(keyTransformer), Right(valueTransformer)) =>
      //       val WrappedToInnerT = wrapperType.applyTypeArg(ToInnerT)

      //       val keyTransformerWithPath =
      //         keyTransformer.target match {
      //           case DerivationTarget.LiftedTransformer(_, _, _) =>
      //             q"""${wrapperErrorPathSupportInstance}.addPath[$toKeyT](
      //                ${keyTransformer.tree},
      //                _root_.io.scalaland.chimney.ErrorPathNode.MapKey($fnK)
      //              )"""
      //           case DerivationTarget.TotalTransformer =>
      //             q"${wrapperSupportInstance}.pure[$toKeyT](${keyTransformer.tree})"
      //         }

      //       val valueTransformerWithPath =
      //         valueTransformer.target match {
      //           case DerivationTarget.LiftedTransformer(_, _, _) =>
      //             q"""${wrapperErrorPathSupportInstance}.addPath[$toValueT](
      //                 ${valueTransformer.tree},
      //                 _root_.io.scalaland.chimney.ErrorPathNode.MapValue($fnK)
      //              )"""
      //           case DerivationTarget.TotalTransformer =>
      //             q"${wrapperSupportInstance}.pure[$toValueT](${valueTransformer.tree})"
      //         }

      //       Right(
      //         q"""${wrapperSupportInstance}.traverse[$To, $WrappedToInnerT, $ToInnerT](
      //             $srcPrefixTree.iterator.map[$WrappedToInnerT] {
      //               case (${fnK.name}: $fromKeyT, ${fnV.name}: $fromValueT) =>
      //                 ${wrapperSupportInstance}.product[$toKeyT, $toValueT](
      //                   $keyTransformerWithPath,
      //                   $valueTransformerWithPath
      //                 )
      //             },
      //             _root_.scala.Predef.identity[$WrappedToInnerT]
      //           )
      //        """
      //       )
      //     case _ =>
      //       Left(keyTransformerE.left.getOrElse(Nil) ++ valueTransformerE.left.getOrElse(Nil))
      //   }
      case _ =>
        expandIterableOrArray[Src, From, To](srcPrefixTree, config)
    }
  }

  @experimental
  def expandIterableOrArray[Src: Type, From: Type, To: Type](using q: Quotes)(
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import topLevelQuotes.reflect._

    val fromInnerT = TypeRepr.of[From].collectionInnerTpe
    val toInnerT = TypeRepr.of[To].collectionInnerTpe

    (fromInnerT.asType, toInnerT.asType) match
      case ('[fromInner], '[toInner]) =>
        resolveRecursiveTransformerBody[fromInner, fromInner, toInner]('{t => t}, config.rec)
          .map {
            // case TransformerBodyTree(
            //     innerTransformerTree,
            //     DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, Some(wrapperErrorPathSupportInstance))
            //     ) =>
            //   val idx = Ident(freshTermName("idx"))

            //   q"""${wrapperSupportInstance}.traverse[$To, ($FromInnerT, _root_.scala.Int), $ToInnerT](
            //     $srcPrefixTree.iterator.zipWithIndex,
            //     { case (${fn.name}: $FromInnerT, ${idx.name}: _root_.scala.Int) =>
            //       ${wrapperErrorPathSupportInstance}.addPath[$ToInnerT](
            //         $innerTransformerTree,
            //         _root_.io.scalaland.chimney.ErrorPathNode.Index($idx)
            //       )
            //     }
            //   )
            //   """
            // case TransformerBodyTree(
            //     innerTransformerTree,
            //     DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, None)
            //     ) =>
            //   q"""${wrapperSupportInstance}.traverse[$To, $FromInnerT, $ToInnerT](
            //     $srcPrefixTree.iterator,
            //     ($fn: $FromInnerT) => $innerTransformerTree
            //   )
            //   """
            case TransformerBodyTree(innerTransformerTree, DerivationTarget.TotalTransformer) =>
              def isTransformationIdentity = '{(f: fromInner) => f} == innerTransformerTree
              def sameCollectionTypes = TypeRepr.of[From].typeSymbol == TypeRepr.of[To].typeSymbol

              val transformedCollectionTree: Expr[Src => To] = (isTransformationIdentity, sameCollectionTypes) match {
                case (true, true) =>
                  // identity transformation, same collection types
                  srcPrefixTree.asExprOf[Src => To]

                case (true, false) =>
                  // identity transformation, different collection types
                  srcPrefixTree.convertCollection[To, toInner]

                case (false, true) =>
                  // non-trivial transformation, same collection types
                  '{(t: Src) => ${unsafeMap('{$srcPrefixTree(t)}, '{(fn: fromInner) => ${innerTransformerTree.asExprOf[fromInner => toInner]}(fn)}).asExprOf[To]}}

                case (false, false) =>
                  '{(t: Src) => ${unsafeIterator('{$srcPrefixTree(t)}).asExprOf[Iterator[fromInner]]}.map((fn: fromInner) => ${innerTransformerTree.asExprOf[fromInner => toInner]}(fn))}
                  .convertCollection[To, toInner]
              }

              config.derivationTarget match {
                // case DerivationTarget.LiftedTransformer(_, wrapperSupportInstance, _) =>
                //   q"${wrapperSupportInstance}.pure($transformedCollectionTree)"
                case DerivationTarget.TotalTransformer =>
                  transformedCollectionTree
              }
          }
  }

  @experimental
  def expandSealedClasses[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {

    sealed trait MatchCaseDef
    case class WildcardMatchCaseDef(inst: Type[Any], rhs: Expr[To]) extends MatchCaseDef
    case class AppliedMatchCaseDef[S](inst: Type[S], rhs: Expr[S => To]) extends MatchCaseDef

    resolveCoproductInstance[Src, From, To](srcPrefixTree, TypeRepr.of[From], config)
      .map { instanceTree =>
        Right(instanceTree)
      }
      .getOrElse {
        val fromCS = TypeRepr.of[From].typeSymbol.classSymbolOpt.get
        val toCS = TypeRepr.of[To].typeSymbol.classSymbolOpt.get

        val fromInstances = fromCS.subclasses.map(_.typeInSealedParent[From])
        val toInstances = toCS.subclasses.map(_.typeInSealedParent[To])

        val targetNamedInstances = toInstances.groupBy(_.typeSymbol.name.toString)

        val instanceClauses: List[Either[Seq[TransformerDerivationError], MatchCaseDef]] = fromInstances.map { instTpe =>
          val instName = instTpe.typeSymbol.name.toString
          given Quotes = topLevelQuotes
          resolveCoproductInstance[Src, From, To](srcPrefixTree, instTpe, config)
            .map { instanceTree =>
              instTpe.asType match
                case '[t] =>
                  Right(
                    AppliedMatchCaseDef[t](Type.of[t], instanceTree.asExprOf[t => To])
                  )
            }
            .getOrElse {
              val instSymbol = instTpe.typeSymbol
              targetNamedInstances.getOrElse(instName, Nil) match {
                case List(matchingTargetTpe)
                    if (instSymbol.flags.is(Flags.Module) || instSymbol.isCaseClass) && matchingTargetTpe.typeSymbol.flags.is(Flags.Module) =>
                  val tree = mkTransformerBodyTree0(config.derivationTarget) {
                    Ref(matchingTargetTpe.typeSymbol.companionModule).asExpr
                  }
                  Right(
                    WildcardMatchCaseDef(instTpe.asType.asInstanceOf[Type[Any]], tree.asExprOf[To])
                  )
                case List(matchingTargetTpe) if instSymbol.isCaseClass && matchingTargetTpe.typeSymbol.isCaseClass =>
                  (instTpe.asType, matchingTargetTpe.asType) match
                    case ('[inst], '[target]) =>
                      expandDestinationCaseClass[inst, inst, target]('{fn => fn}, config.rec)
                        .map { innerTransformerTree =>
                          AppliedMatchCaseDef(Type.of[inst], innerTransformerTree.asExprOf[inst => To])
                          // cq"$fn: $instTpe => $innerTransformerTree"
                        }
                case _ :: _ :: _ =>
                  Left {
                    Seq(
                      AmbiguousCoproductInstance(
                        instName,
                        TypeRepr.of[From].typeSymbol.fullName,
                        TypeRepr.of[To].typeSymbol.fullName
                      )
                    )
                  }
                case _ =>
                  Left {
                    Seq(
                      CantFindCoproductInstanceTransformer(
                        instSymbol.fullName,
                        TypeRepr.of[From].typeSymbol.fullName,
                        TypeRepr.of[To].typeSymbol.fullName
                      )
                    )
                  }
              }
            }
        }

        if (instanceClauses.forall(_.isRight)) {
          val clauses = instanceClauses.collect { case Right(clause) => clause }
          def insertMatch(using Quotes)(selector: Expr[Any], clauses: List[MatchCaseDef]): Expr[To] = {
            import quotes.reflect._
            Match(selector.asTerm, clauses.map {
              case WildcardMatchCaseDef(inst, rhs) => 
                inst match 
                  case '[t] => CaseDef(Typed(Wildcard(), TypeTree.of[t]), None, rhs.asTerm)
              case AppliedMatchCaseDef(inst, rhs) =>
                inst match
                  case '[t] =>
                    val bind = Symbol.newBind(Symbol.spliceOwner, "nm", Flags.EmptyFlags, TypeRepr.of[t])
                    CaseDef(Bind(bind, Typed(Ref(bind), TypeTree.of[t])), None, Apply(Select.unique(rhs.asTerm, "apply"), List(Ref(bind))))
            }).asExprOf[To]
          }
          Right {
            given Quotes = topLevelQuotes
            '{(t: Src) => ${insertMatch('{$srcPrefixTree(t)}, clauses)}}.asExprOf[Src => To]
          }
        } else {
          Left {
            instanceClauses.collect { case Left(derivationErrors) => derivationErrors }.flatten
          }
        }
      }

  }

  def resolveCoproductInstance[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      instType: topLevelQuotes.reflect.TypeRepr,
      config: TransformerConfig
  ): Option[Expr[Src => To]] = {
    val correctedInstTypeSymbol = 
      if(instType.typeSymbol.moduleClass == Symbol.noSymbol) instType.typeSymbol
      else instType.typeSymbol.moduleClass
    config.derivationTarget match {
      // case DerivationTarget.LiftedTransformer(_, _, _) if config.coproductInstancesF.contains((From.typeSymbol, To)) =>
      //   Some(
      //     mkCoproductInstance(
      //       config.transformerDefinitionPrefix,
      //       srcPrefixTree,
      //       From.typeSymbol,
      //       To,
      //       config.derivationTarget
      //     )
      //   )
      case _ if config.coproductInstances.contains((correctedInstTypeSymbol, TypeRepr.of[To])) =>
        import topLevelQuotes._
        given Quotes = topLevelQuotes
        Some(
          // mkTransformerBodyTree0(config.derivationTarget) {
            mkCoproductInstance[Src, From, To](using topLevelQuotes)(
              config.transformerDefinitionPrefix.get,
              srcPrefixTree,
              correctedInstTypeSymbol,
              DerivationTarget.TotalTransformer
            ).asExprOf[Src => To]
          // }
        )
      case _ =>
        None
    }
  }

  @experimental
  def expandDestinationTuple[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {

    resolveSourceTupleAccessors[From, To]
      .flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping[Src, From, To](srcPrefixTree, accessorsMapping, config)
      }
      .map { transformerBodyPerTarget =>
        val targets = TypeRepr.of[To].caseClassParams.map(Target.fromField[To](_))
        val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
        
        mkTransformerBodyTree[Src, To](targets, bodyTreeArgs, config.derivationTarget) { args =>
          '{ (t: Src) => ${ mkNewTupleClass[To](applyToAll(args, 't)) } }
        }
      }
  }

  @experimental
  def expandDestinationCaseClass[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Expr[Src => To]] = {
    import topLevelQuotes.reflect._

    val targets = TypeRepr.of[To].caseClassParams.map(Target.fromField[To](_))
    
    given Quotes = topLevelQuotes
    val targetTransformerBodiesMapping = if (isTuple[From]) {
      resolveSourceTupleAccessors[From, To].flatMap { accessorsMapping =>
        resolveTransformerBodyTreeFromAccessorsMapping[Src, From, To](srcPrefixTree, accessorsMapping, config)
      }
    } else {
      val overridesMapping = resolveOverrides[Src, From](srcPrefixTree, targets, config)
      val notOverridenTargets = targets.diff(overridesMapping.keys.toSeq)
      val accessorsMapping = resolveAccessorsMapping[From](notOverridenTargets, config)

      resolveTransformerBodyTreeFromAccessorsMapping[Src, From, To](srcPrefixTree, accessorsMapping, config)
          .map(_ ++ overridesMapping)
    }
  
    targetTransformerBodiesMapping.map { transformerBodyPerTarget =>
      val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
      given Quotes = topLevelQuotes

      mkTransformerBodyTree[Src, To](targets, bodyTreeArgs, config.derivationTarget) { args =>
        '{ (t: Src) => ${ mkNewClass[To](applyToAll(args, 't)) } }
      }
    }
  }

  def applyToAll[Src: Type](using Quotes)(args: Seq[Expr[Src => Any]], t: Expr[Src]): Seq[Expr[Any]] = 
    args.map { arg =>
      '{$arg($t)}
    }


  // def expandDestinationJavaBean(
  //     srcPrefixTree: Tree,
  //     config: TransformerConfig
  // )(From: Type, To: Type): Either[Seq[TransformerDerivationError], Tree] = {

  //   val beanSetters = To.beanSetterMethods
  //   val targets = beanSetters.map(Target.fromJavaBeanSetter(_, To))

  //   val accessorsMapping = resolveAccessorsMapping(From, targets, config)

  //   resolveTransformerBodyTreeFromAccessorsMapping(srcPrefixTree, accessorsMapping, From, To, config)
  //     .map { transformerBodyPerTarget =>
  //       val bodyTreeArgs = targets.map(target => transformerBodyPerTarget(target))
  //       mkTransformerBodyTree(To, targets, bodyTreeArgs, config.derivationTarget) { args =>
  //         mkNewJavaBean(To, targets zip args)
  //       }
  //     }
  // }

  @experimental
  def resolveTransformerBodyTreeFromAccessorsMapping[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      accessorsMapping: Map[Target, AccessorResolution],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], Map[Target, TransformerBodyTree]] = {
    given Quotes = topLevelQuotes

    val (erroredTargets, resolvedBodyTrees) = accessorsMapping.map {
      case (target, accessor: AccessorResolution.Resolved) =>
        target -> resolveTransformerBodyTreeFromAccessor[Src, From](srcPrefixTree, target, accessor, config)
      case (target, accessor) =>
        target -> Left(
          Seq(
            MissingAccessor(
              fieldName = target.name,
              fieldTypeName = 
                target.tpe match
                  case '[t] => TypeRepr.of[t].fqName,
              sourceTypeName = TypeRepr.of[From].fqName,
              targetTypeName = TypeRepr.of[To].fqName,
              defAvailable = accessor == AccessorResolution.DefAvailable
            )
          )
        )
    }.partitionEitherValues

    if (erroredTargets.isEmpty) {
      Right(resolvedBodyTrees)
    } else {
      val targetsToFallback = erroredTargets.collect {
        case (target, _) if !accessorsMapping(target).isResolved => target
      }
      val fallbackTransformerBodies = resolveFallbackTransformerBodies[To](targetsToFallback, config)
      val unresolvedTargets = accessorsMapping.keys.toList
        .diff(resolvedBodyTrees.keys.toList)
        .diff(fallbackTransformerBodies.keys.toList)

      if (unresolvedTargets.isEmpty) {
        Right(resolvedBodyTrees ++ fallbackTransformerBodies)
      } else {
        val errors = unresolvedTargets.flatMap { target =>
          accessorsMapping(target) match {
            case AccessorResolution.Resolved(symbol: Symbol, _) =>
              erroredTargets(target) :+ MissingTransformer(
                fieldName = target.name,
                sourceFieldTypeName = 
                  resultTypeIn[From](using topLevelQuotes)(symbol) match
                    case '[t] => TypeRepr.of[t].fqName,
                targetFieldTypeName = target.tpe match
                  case '[t] => TypeRepr.of[t].fqName,
                sourceTypeName = TypeRepr.of[From].fqName,
                targetTypeName = TypeRepr.of[To].fqName
              )
            case _ => erroredTargets(target)
          }
        }
        Left(errors)
      }
    }
  }

  @experimental
  def resolveTransformerBodyTreeFromAccessor[Src: Type, From: Type](
      srcPrefixTree: Expr[Src => From],
      target: Target,
      accessor: AccessorResolution.Resolved,
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
    given Quotes = topLevelQuotes
    val resolved =
      (resultTypeIn[From](using topLevelQuotes)(accessor.symbol), target.tpe) match
        case ('[from], '[to]) =>
          resolveRecursiveTransformerBody[Src, from, to](
            '{(t: Src) => ${unsafeSelectByName[From, from]('{$srcPrefixTree(t)}, accessor.symbol.name)} },
            config
          )

    (resolved, config.derivationTarget) match {
      // case (Right(bodyTree), DerivationTarget.LiftedTransformer(_, _, Some(errorPathSupport)))
      //     if bodyTree.isLiftedTarget =>
      //   Right {
      //     TransformerBodyTree(
      //       q"""$errorPathSupport.addPath[${target.tpe}](
      //            ${bodyTree.tree},
      //            _root_.io.scalaland.chimney.ErrorPathNode.Accessor(${accessor.symbol.name.toString})
      //          )""",
      //       config.derivationTarget
      //     )
      //   }
      case _ => resolved
    }
  }

  @experimental
  def resolveRecursiveTransformerBody[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
    resolveTransformerBody[Src, From, To](srcPrefixTree, config.rec)
  }

  @experimental
  def resolveTransformerBody[Src: Type, From: Type, To: Type](
      srcPrefixTree: Expr[Src => From],
      config: TransformerConfig
  ): Either[Seq[TransformerDerivationError], TransformerBodyTree] = {
    config.derivationTarget match {
      // case DerivationTarget.LiftedTransformer(wrapperType, _, _) =>
      //   val implicitTransformerF = resolveImplicitTransformer(config)(From, To)
      //   val implicitTransformer = findLocalImplicitTransformer(From, To, DerivationTarget.TotalTransformer)

      //   (implicitTransformerF, implicitTransformer) match {
      //     case (Some(localImplicitTreeF), Some(localImplicitTree)) =>
      //       c.abort(
      //         c.enclosingPosition,
      //         s"""Ambiguous implicits while resolving Chimney recursive transformation:
      //            |
      //            |TransformerF[${wrapperType}, $From, $To]: $localImplicitTreeF
      //            |Transformer[$From, $To]: $localImplicitTree
      //            |
      //            |Please eliminate ambiguity from implicit scope or use withFieldComputed/withFieldComputedF to decide which one should be used
      //            |""".stripMargin
      //       )
      //     case (Some(localImplicitTreeF), None) =>
      //       Right(TransformerBodyTree(localImplicitTreeF.callTransform(srcPrefixTree), config.derivationTarget))
      //     case (None, Some(localImplicitTree)) =>
      //       Right(
      //         TransformerBodyTree(localImplicitTree.callTransform(srcPrefixTree), DerivationTarget.TotalTransformer)
      //       )
      //     case (None, None) =>
      //       deriveTransformerTree(srcPrefixTree, config)(From, To)
      //         .map(tree => TransformerBodyTree(tree, config.derivationTarget))
      //   }
      case DerivationTarget.TotalTransformer =>
        expandTransformerTree[Src, From, To](srcPrefixTree, config)
          .map(tree => TransformerBodyTree(tree, config.derivationTarget))
    }
  }

  def resolveImplicitTransformer[From: Type, To: Type](config: TransformerConfig): Option[Expr[Transformer[From, To]]] = {
    val fromTS = TypeRepr.of[From].typeSymbol
    val toTS = TypeRepr.of[To].typeSymbol
    if (config.definitionScope.contains((fromTS, toTS))) {
      None
    } else {
      findLocalImplicitTransformer[From, To](config.derivationTarget)
    }
  }

  def findLocalTransformerConfigurationFlags: Expr[io.scalaland.chimney.dsl.TransformerConfiguration[_ <: io.scalaland.chimney.internal.TransformerFlags]] = {
    import quotes.reflect._
    inferImplicitTpe[io.scalaland.chimney.dsl.TransformerConfiguration[_ <: io.scalaland.chimney.internal.TransformerFlags]](macrosDisabled = true)
      .getOrElse {
        // $COVERAGE-OFF$
        report.errorAndAbort("Can't locate implicit TransformerConfiguration!", Position.ofMacroExpansion)
        // $COVERAGE-ON$
      }
  }

  private def findLocalImplicitTransformer[From: Type, To: Type](derivationTarget: DerivationTarget): Option[Expr[Transformer[From, To]]] = {
    import quotes.reflect._
    (derivationTarget match {
      // case DerivationTarget.LiftedTransformer(f, _, _) =>
      //   '{_root_.io.scalaland.chimney.TransformerF[$f, $From, $To]}
      case DerivationTarget.TotalTransformer =>
        inferImplicitTpe[Transformer[From, To]](macrosDisabled = false)
        // inferImplicitTpe(searchTypeTree, macrosDisabled = false)
    }).filterNot(expr => isDeriving(expr.asTerm))
  }

  // def findTransformerErrorPathSupport(wrapperType: Type): Option[Tree] = {
  //   inferImplicitTpe(tq"_root_.io.scalaland.chimney.TransformerFErrorPathSupport[$wrapperType]", macrosDisabled = true)
  // }

  private def inferImplicitTpe[T: Type](macrosDisabled: Boolean): Option[Expr[T]] = {// macrosDisabled unused here 
    Expr.summon[T]
  }
  
  private def isDeriving(tree: quotes.reflect.Term): Boolean = {
    import quotes.reflect._
    tree match {
      case Inlined(Some(TypeApply(Ident(derive),_)),_,Typed(_,tpeTree)) =>
        derive == "derive" && tpeTree.symbol == TypeRepr.of[Transformer].typeSymbol
      case _ => false
    }
  }

  private def notSupportedDerivation[Src: Type, From: Type, To: Type](srcPrefixTree: Expr[Src => From]): Left[Seq[NotSupportedTransformerDerivation], Nothing] = {
    import topLevelQuotes.reflect._
    Left {
      Seq(
        NotSupportedTransformerDerivation(
          srcPrefixTree.show, // toFieldName(srcPrefixTree), // TODO - needs redesigning
          TypeRepr.of[From].fqName,
          TypeRepr.of[To].fqName
        )
      )
    }
  }

  private val chimneyDocUrl = "https://scalalandio.github.io/chimney"
}
