package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.quoted

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override type Type[T] = quoted.Type[T]

  object Type extends TypeModule {

    object platformSpecific {

      @scala.annotation.tailrec
      def returnType[A](typeRepr: TypeRepr): Type[A] = typeRepr.widenByName match {
        case MethodType(_, _, out) => returnType[A](out)
        case out                   => out.asType.asInstanceOf[Type[A]]
      }

      def resolveTypeArgsForMethodArguments(tpe: TypeRepr, method: Symbol): (Map[String, TypeRepr], List[TypeRepr]) =
        tpe.memberType(method) match {
          // monomorphic
          case MethodType(names, types, _) =>
            val typeArgs: List[TypeRepr] = Nil
            val typeArgumentByName: Map[String, TypeRepr] = names.zip(types).toMap
            typeArgumentByName -> typeArgs
          // polymorphic
          case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
            // TODO: check if types of constructor match types passed to tpe
            val typeArgs: List[TypeRepr] = tpe.typeArgs
            val typeArgumentByAlias = typeRefs.zip(typeArgs).toMap
            val typeArgumentByName: Map[String, TypeRepr] = names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
            typeArgumentByName -> typeArgs
          // unknown
          case tpe =>
            assertionFailed(
              s"Constructor of ${Type.prettyPrint(tpe.asType.asInstanceOf[Type[Any]])} has unrecognized/unsupported format of type: ${tpe}"
            )
        }
    }

    val Nothing: Type[Nothing] = quoted.Type.of[Nothing]
    val Any: Type[Any] = quoted.Type.of[Any]
    val AnyVal: Type[AnyVal] = quoted.Type.of[AnyVal]
    val Boolean: Type[Boolean] = quoted.Type.of[Boolean]
    val Byte: Type[Byte] = quoted.Type.of[Byte]
    val Char: Type[Char] = quoted.Type.of[Char]
    val Short: Type[Short] = quoted.Type.of[Short]
    val Int: Type[Int] = quoted.Type.of[Int]
    val Long: Type[Long] = quoted.Type.of[Long]
    val Float: Type[Float] = quoted.Type.of[Float]
    val Double: Type[Double] = quoted.Type.of[Double]
    val Unit: Type[Unit] = quoted.Type.of[Unit]

    def Tuple2[A: Type, B: Type]: Type[(A, B)] = quoted.Type.of[(A, B)]

    def Function1[A: Type, B: Type]: Type[A => B] = quoted.Type.of[A => B]
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C] = quoted.Type.of[(A, B) => C]

    object Array extends ArrayModule {
      def apply[T: Type]: Type[Array[T]] = quoted.Type.of[Array[T]]
    }

    object Option extends OptionModule {

      def apply[T: Type]: Type[Option[T]] = quoted.Type.of[Option[T]]
      def unapply[T](tpe: Type[T]): Option[ComputedType] = tpe match {
        case '[Option[inner]] => Some(ComputedType(Type[inner]))
        case _                => scala.None
      }

      val None: Type[scala.None.type] = quoted.Type.of[scala.None.type]
    }

    object Either extends EitherModule {
      def apply[L: Type, R: Type]: Type[Either[L, R]] = quoted.Type.of[Either[L, R]]
      def Left[L: Type, R: Type]: Type[Left[L, R]] = quoted.Type.of[Left[L, R]]
      def Right[L: Type, R: Type]: Type[Right[L, R]] = quoted.Type.of[Right[L, R]]
    }

    def isSubtypeOf[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) <:< TypeRepr.of(using B)
    def isSameAs[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) =:= TypeRepr.of(using B)

    def isSealed[A](A: Type[A]): Boolean = {
      val flags = TypeRepr.of(using A).typeSymbol.flags
      flags.is(Flags.Enum) || flags.is(Flags.Sealed)
    }

    def prettyPrint[T: Type]: String = {
      val repr = TypeRepr.of[T]
      scala.util.Try(repr.dealias.show(using Printer.TypeReprAnsiCode)).getOrElse(repr.toString)
    }
  }
}
