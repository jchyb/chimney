package io.scalaland.chimney.internal.utils

import scala.quoted._
import scala.annotation.tailrec
import scala.annotation.experimental

trait TypeTestUtils extends MacroUtils {

  def isSubtype[From: Type, To: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[From] <:< TypeRepr.of[To]
  }

  @experimental
  private def isValueClassReturnTypeEqual[ValueClass: Type, RetType: Type]: Boolean = {
    import topLevelQuotes.reflect._

    @tailrec
    def methodResultType(typeRepr: TypeRepr): TypeRepr = typeRepr match
      case MethodType(_, _, retType) => methodResultType(retType)
      case retType => retType
    
    TypeRepr.of[ValueClass].valueClassMember.exists{ a =>
      methodResultType(a.widen) =:= TypeRepr.of[RetType]
    }
  }

  @experimental
  def fromValueClassToType[From: Type, To: Type]: Boolean = {
    import topLevelQuotes.reflect._
    TypeRepr.of[From].isValueClass && isValueClassReturnTypeEqual[From, To]
    // TypeRepr.of[From].isValueClass && from.valueClassMember.exists(_.returnType =:= to)
  }

  @experimental
  def fromTypeToValueClass[From: Type, To: Type]: Boolean = {
    import topLevelQuotes.reflect._
    TypeRepr.of[To].isValueClass && isValueClassReturnTypeEqual[To, From]
    // to.isValueClass && to.valueClassMember.exists(_.returnType =:= from)
  }

  def isOption[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[T] <:< TypeRepr.of[Option[_]]
  }

  def bothOptions[From: Type, To: Type](using Quotes): Boolean = {
    isOption[From] && isOption[To]
  }

  def bothEithers[From: Type, To: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[From] <:< TypeRepr.of[Either[_, _]] && TypeRepr.of[To] <:< TypeRepr.of[Either[_, _]]
  }

  def bothOfIterableOrArray[From: Type, To: Type](using Quotes): Boolean = {
    iterableOrArray[From] && iterableOrArray[To]
  }

  def isTuple[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[T].isTupleN
  }

  def isUnit[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[T] <:< TypeRepr.of[Unit]
  }

  def destinationCaseClass[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    val sym = TypeRepr.of[T].typeSymbol
    val flags = sym.flags
    flags.is(Flags.Case) && sym.isClassDef
  }

  // def destinationJavaBean(to: Type): Boolean = {
  //   if (to.typeSymbol.isClass) {
  //     val primaryConstructor = to.typeSymbol.asClass.primaryConstructor
  //     primaryConstructor.isPublic &&
  //     primaryConstructor.isMethod &&
  //     primaryConstructor.asMethod.paramLists == List(Nil) &&
  //     to.beanSetterMethods.nonEmpty
  //   } else {
  //     // $COVERAGE-OFF$
  //     false
  //     // $COVERAGE-ON$
  //   }
  // }

  def bothSealedClasses[From: Type, To: Type]: Boolean = {
    import topLevelQuotes.reflect._
    TypeRepr.of[From].isSealedClass && TypeRepr.of[To].isSealedClass
  }

  def iterableOrArray[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[T] <:< iterableTpe || TypeRepr.of[T] <:< arrayTpe
  }

  def isMap[T: Type](using Quotes): Boolean = {
    import quotes.reflect._
    TypeRepr.of[T] <:< mapTpe
  }

  def optionTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Option[_]]
  def someTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Some[_]]
  def noneTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[None.type]
  def eitherTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Either[_, _]]
  def leftTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Left[_, _]]
  def rightTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Right[_, _]]
  def iterableTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Iterable[_]]
  def arrayTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Array[_]]
  def mapTpe(using Quotes): quotes.reflect.TypeRepr = quotes.reflect.TypeRepr.of[Map[_, _]]
}
