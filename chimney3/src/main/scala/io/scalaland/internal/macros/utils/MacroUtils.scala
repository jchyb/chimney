package io.scalaland.chimney.internal.utils

import scala.quoted._
import scala.annotation.experimental
import scala.annotation.targetName

trait MacroUtils extends QuotationUtils {

  val topLevelQuotes: Quotes

  def extractSelectorFieldName[To: Type, T: Type](tree: Expr[To => T])(using Quotes): String = {
    import quotes.reflect._
    extractSelectorFieldNameOpt(tree.asTerm).getOrElse {
      quotes.reflect.report.errorAndAbort(invalidSelectorErrorMessage(tree), quotes.reflect.Position.ofMacroExpansion)
    }
  }

  def extractSelectorFieldNameOpt(using Quotes)(term: quotes.reflect.Term): Option[String] = {
    import quotes.reflect._

    object SelectLike {
      def unapply(term: Term): Option[(String, String)] =
        term match {
          case Select(Ident(out),va) => Some(out, va)
          case Block(_, SelectLike(ident, member)) => Some(ident, member)
          case _ => None
        }
    }

    term match
      case Inlined(_, _, Block(
              List(DefDef(_,List(List(ValDef(in,_,_))),_,Some(SelectLike(out, va)))),
              _ // closure (necessary?)
            )) if in == out => Some(va)
      case Inlined(_, _, block) => extractSelectorFieldNameOpt(block)
      case _ => None
  }

  private def invalidSelectorErrorMessage[T](selectorTree: Expr[T]): String = {
    s"Invalid selector expression: ${selectorTree.show}"
  }

  extension (s: String) {
    def toSingletonTpe: topLevelQuotes.reflect.ConstantType = {
      import topLevelQuotes.reflect._
      ConstantType(StringConstant(s))
    }
  }

  extension (t: topLevelQuotes.reflect.TypeRepr){
    def isCaseClass: Boolean =
      t.typeSymbol.isCaseClass
    
    @experimental
    def isValueClass: Boolean = {
      import topLevelQuotes.reflect._
      t <:< defn.AnyValClass.typeRef && !primitives.exists(_ =:= t)
    }
    
    def isSealedClass: Boolean = {
      import topLevelQuotes.reflect._
      t.typeSymbol.flags.is(Flags.Sealed) && t.typeSymbol.isClassDef
    }
    
    def applyTypeArgs(args: topLevelQuotes.reflect.TypeRepr*): topLevelQuotes.reflect.TypeRepr = {
      import topLevelQuotes.reflect._
      t.appliedTo(args.toList)
    }

    private def tupleParams: List[topLevelQuotes.reflect.Symbol] = {
      import topLevelQuotes.reflect._
      t.typeSymbol.methodMembers.sortBy(_.name).collect { // had to sort, otherwise we would get weird orderings etc.
        case m: Symbol if m.flags.is(Flags.CaseAccessor) && m.flags.is(Flags.Method) => m
      }
    }

    def fqName: String =
      t.typeSymbol.fullName.replaceAllLiterally("_$", "").replaceAllLiterally("$", "")

    @experimental
    def caseClassParams: List[topLevelQuotes.reflect.Symbol] = {
      import topLevelQuotes.reflect._
      if t.isTupleN then 
        t.tupleParams
      else 
        t.typeSymbol.caseFields
    }

    def getterMethods: Seq[topLevelQuotes.reflect.Symbol] = {
      import topLevelQuotes.reflect._
      val sym = t.typeSymbol
      (sym.fieldMembers ++ sym.declaredMethods).collect {
        case m: Symbol if !m.flags.is(Flags.Private) && !m.flags.is(Flags.Protected) 
          && (m.flags.is(Flags.CaseAccessor) || m.paramSymss.isEmpty) =>
          m
      }.toSeq
    }

    def singletonString: String = {
      import topLevelQuotes.reflect._
      val ConstantType(v) = t
      v.value.toString
    }

    @experimental
    def collectionInnerTpe: topLevelQuotes.reflect.TypeRepr = {
      t.typeArgs match {
        case List(unaryInnerT) => unaryInnerT
        case List(innerT1, innerT2) =>
          given Quotes = topLevelQuotes
          (innerT1.asType, innerT2.asType) match
            case ('[t1], '[t2]) =>
              topLevelQuotes.reflect.TypeRepr.of[(t1, t2)]
        // $COVERAGE-OFF$
        case Nil =>
          topLevelQuotes.reflect.report.errorAndAbort("Collection type must have type parameters!")
        case _ =>
          topLevelQuotes.reflect.report.errorAndAbort("Collection types with more than 2 type arguments are not supported!")
        // $COVERAGE-ON$
      }
    }

    def valueClassMember: Option[topLevelQuotes.reflect.TypeRepr] = {
      import topLevelQuotes.reflect._
      val ts = t.widen.typeSymbol
      (ts.fieldMembers ++ ts.declaredMethods).headOption.map(Ref(_).tpe)
    }

    // def beanSetterMethods: Seq[MethodSymbol] = {
    //   t.members.collect { case m: MethodSymbol if m.isBeanSetter => m }.toSeq
    // }
  }
  
  extension (s: topLevelQuotes.reflect.Symbol) {
    @targetName("Symbol_isCaseClass")
    def isCaseClass: Boolean = {
      import topLevelQuotes.reflect._
      s.flags.is(Flags.Case) && s.isClassDef
    }

    def classSymbolOpt: Option[topLevelQuotes.reflect.Symbol] = {
      import topLevelQuotes.reflect._
      if (s.isClassDef) Some(s) else None
    }
      
    def caseClassDefaults: Map[String, Expr[Any]] = {
      import topLevelQuotes.reflect._
      given Quotes = topLevelQuotes
      classSymbolOpt
        .map { classSymbol =>
          val companionModule = classSymbol.companionClass
          val mod = Ref(classSymbol.companionModule)
          val names =
            for p <- classSymbol.caseFields if p.flags.is(Flags.HasDefault)
            yield p.name
          val namesExpr: Expr[List[String]] =
            Expr.ofList(names.map(Expr(_)))
          
          val body = companionModule.tree.asInstanceOf[ClassDef].body
          val idents: List[Expr[Any]] =
            for case deff @ DefDef(name, _, _, _) <- body
            if name.startsWith("$lessinit$greater$default")
            yield mod.select(deff.symbol).asExprOf[Any]
          
          names.zip(idents).toMap
        }
    }.getOrElse(Map.empty)

    def subclasses: List[topLevelQuotes.reflect.Symbol] =
      import topLevelQuotes.reflect._
      s.children.flatMap { subclass =>
        if (subclass.flags.is(Flags.Trait | Flags.Sealed)) {
          subclass.subclasses
        } else {
          List(subclass)
        }
      }

    @experimental
    def typeInSealedParent[ParentTpe: Type]: topLevelQuotes.reflect.TypeRepr = {
      import topLevelQuotes.reflect._
      // TypeRepr.of[ParentTpe].memberType(s).widen
      s.typeRef
    }
  }

  // implicit class MethodSymbolOps(ms: MethodSymbol) {

  //   def canonicalName: String = {
  //     val name = ms.name.decodedName.toString
  //     if (isBeanSetter) {
  //       val stripedPrefix = name.drop(3)
  //       val lowerizedName = stripedPrefix.toCharArray
  //       lowerizedName(0) = lowerizedName(0).toLower
  //       new String(lowerizedName)
  //     } else {
  //       name
  //     }
  //   }

  //   def isBeanSetter: Boolean = {
  //     ms.isPublic &&
  //     ms.name.decodedName.toString.startsWith("set") &&
  //     ms.name.decodedName.toString.lengthCompare(3) > 0 &&
  //     ms.paramLists.lengthCompare(1) == 0 &&
  //     ms.paramLists.head.lengthCompare(1) == 0 &&
  //     ms.returnType == typeOf[Unit]
  //   }

  @experimental
  @scala.annotation.tailrec
  private def methodResultType(using Quotes)(typeRepr: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr = {
    import quotes.reflect._
    typeRepr match
      case MethodType(_, _, retType) => methodResultType(retType)
      case retType => retType.widen
  }

  @experimental
  def resultTypeIn[T: Type](using Quotes)(symbol: quotes.reflect.Symbol): Type[Any] = {
    import quotes.reflect._
    methodResultType(TypeRepr.of[T].memberType(symbol)).asType.asInstanceOf[Type[Any]]
  }

  @experimental
  def resultTypeIn(t: topLevelQuotes.reflect.TypeRepr)(symbol: topLevelQuotes.reflect.Symbol) = {
    methodResultType(using topLevelQuotes)(t.memberType(symbol)).asType.asInstanceOf[Type[Any]]
  }

  //   def beanSetterParamTypeIn(site: Type): Type = {
  //     ms.paramLists.head.head.typeSignatureIn(site)
  //   }

  //   def isParameterless: Boolean = {
  //     ms.paramLists.isEmpty || ms.paramLists == List(List())
  //   }
  // }

  // // $COVERAGE-OFF$
  // implicit class TreeOps(t: Tree) {

  //   def debug: Tree = {
  //     println("TREE: " + t)
  //     println("RAW:  " + showRaw(t))
  //     t
  //   }

  //   def extractBlock: (List[Tree], Tree) = t match {
  //     case Typed(tt, _) =>
  //       tt.extractBlock
  //     case Block(stats, expr) =>
  //       (stats, expr)
  //     case other =>
  //       (Nil, other)
  //   }

  //   def extractStats: List[Tree] = t match {
  //     case Typed(tt, _) =>
  //       tt.extractStats
  //     case Block(stats, _) =>
  //       stats
  //     case _ =>
  //       Nil
  //   }

  //   def insertToBlock(tree: Tree): Tree = {
  //     val (stats, expr) = t.extractBlock
  //     Block(stats :+ tree, expr)
  //   }

  //   def extractSelectorFieldName: TermName = {
  //     extractSelectorFieldNameOpt.getOrElse {
  //       c.abort(c.enclosingPosition, invalidSelectorErrorMessage(t))
  //     }
  //   }

  //   def extractSelectorFieldNameOpt: Option[TermName] = {
  //     t match {
  //       case q"(${vd: ValDef}) => ${idt: Ident}.${fieldName: TermName}" if vd.name == idt.name =>
  //         Some(fieldName)
  //       case _ =>
  //         None
  //     }
  //   }

  extension [Src: Type, From: Type](expr: Expr[Src => From])
    @experimental
    def convertCollection[TargetTpe: Type, InnerTpe: Type](using Quotes): Expr[Src => TargetTpe] = {
      import quotes.reflect._
      if (TypeRepr.of[TargetTpe] <:< TypeRepr.of[scala.collection.Map[_, _]]) {
        '{ (t: Src) => ${unsafeToMap[From, TargetTpe]('{$expr(t)})} }.asExprOf[Src => TargetTpe]
      } else {
        '{ (t: Src) => ${unsafeTo('{$expr(t)}, '{scala.compiletime.summonInline[scala.collection.compat.Factory[InnerTpe, TargetTpe]]})} }
      }
    }


  // INFO completely changed - name not really applicable anymore
  def callUnaryApply[Src: Type, Arg: Type, Res: Type](src: Expr[Arg => Res], argTree: Expr[Src => Arg]): Expr[Src => Res] = {
    '{ (t: Src) => ${src}.apply($argTree.apply(t)) }
  }
  
  extension [T, U] (pair: (Expr[T], Expr[U]))
    def extractSelectorsOrAbort: (String, String) = {
      import topLevelQuotes.reflect._
      val (selectorTree1, selectorTree2) = pair
      (extractSelectorFieldNameOpt(using topLevelQuotes)(selectorTree1.asTerm), extractSelectorFieldNameOpt(using topLevelQuotes)(selectorTree2.asTerm)) match {
        case (Some(fieldName1), Some(fieldName2)) =>
          (fieldName1, fieldName2)
        case (None, Some(_)) =>
          report.errorAndAbort(invalidSelectorErrorMessage(selectorTree1), Position.ofMacroExpansion)
        case (Some(_), None) =>
          report.errorAndAbort(invalidSelectorErrorMessage(selectorTree2), Position.ofMacroExpansion)
        case (None, None) =>
          val err1 = invalidSelectorErrorMessage(selectorTree1)
          val err2 = invalidSelectorErrorMessage(selectorTree2)
          report.errorAndAbort(s"Invalid selectors:\n$err1\n$err2", Position.ofMacroExpansion)
      }
    }

  // $COVERAGE-ON$
  given Quotes = topLevelQuotes
  import topLevelQuotes.reflect._
  private val primitives = Set(
    TypeRepr.of[Double],
    TypeRepr.of[Float],
    TypeRepr.of[Short],
    TypeRepr.of[Byte],
    TypeRepr.of[Int],
    TypeRepr.of[Long],
    TypeRepr.of[Char],
    TypeRepr.of[Boolean],
    TypeRepr.of[Unit]
  )
}
