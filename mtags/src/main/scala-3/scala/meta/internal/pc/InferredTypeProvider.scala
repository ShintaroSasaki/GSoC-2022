package scala.meta.internal.pc

import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.tokens.{Token => T}

import org.eclipse.lsp4j.TextEdit
import java.nio.file.Paths
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.interactive.Interactive
import java.net.URI
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Types._
import scala.meta.internal.mtags.MtagsEnrichments._
import dotty.tools.dotc.core.Symbols.NoSymbol
import scala.meta as m
import scala.meta.internal.pc.AutoImports.AutoImportsGenerator
import dotty.tools.dotc.ast.untpd

/**
 * Tries to calculate edits needed to insert the inferred type annotation
 * in all the places that it is possible such as:
 * - value or variable declaration
 * - methods
 * - pattern matches
 * - for comprehensions
 * - lambdas
 *
 * The provider will not check if the type does not exist, since there is no way to
 * get that data from the presentation compiler. The actual check is being done via
 * scalameta parser in InsertInferredType code action.
 *
 * @param params position and actual source
 * @param driver Scala 3 interactive compiler driver
 * @param config presentation compielr configuration
 */
final class InferredTypeProvider(
    params: OffsetParams,
    driver: InteractiveDriver,
    config: PresentationCompilerConfig
) {

  def inferredTypeEdits(): List[TextEdit] = {
    val uri = params.uri
    val filePath = Paths.get(uri)
    val source = SourceFile.virtual(filePath.toString, params.text)
    driver.run(uri, source)
    val unit = driver.currentCtx.run.units.head
    val pos = driver.sourcePosition(params)
    val path =
      Interactive.pathTo(driver.openedTrees(uri), pos)(using driver.currentCtx)

    given locatedCtx: Context = driver.localContext(params)
    val symbolPrinter = new SymbolPrinter()
    val namesInScope = NamesInScope.build(unit.tpdTree)
    val autoImportsGen = AutoImports.generator(
      pos,
      params.text,
      unit.tpdTree,
      namesInScope,
      config
    )
    val history = new ShortenedNames(locatedCtx)

    /*
     * Get the exact position in ValDef pattern for val (a, b) = (1, 2)
     * Suprisingly, val ((a, c), b) = ((1, 3), 2) will be covered by Bind
     * https://github.com/lampepfl/dotty/issues/12627
     */
    def findTuplePart(
        applied: AppliedType,
        metaPattern: m.Pat,
        valdefOffset: Int
    )(using ctx: Context) = {
      import scala.meta._
      metaPattern match {
        case tpl: m.Pat.Tuple =>
          val newOffset = params.offset - valdefOffset + 4
          val tupleIndex = tpl.args.indexWhere { p =>
            p.pos.start <= newOffset && p.pos.end >= newOffset
          }
          if (tupleIndex >= 0) {
            val tuplePartTpe = applied.args(tupleIndex)
            val imports =
              autoImportsGen
                .forSymbol(tuplePartTpe.typeSymbol)
                .getOrElse(Nil)
            val short = shortType(tuplePartTpe, history)
            val typeEndPos = tpl.args(tupleIndex).pos.end
            val namePos = typeEndPos + valdefOffset - 4
            val lspPos = driver.sourcePosition(params.uri, namePos).toLSP
            val typeNameEdit =
              new TextEdit(
                lspPos,
                ": " + symbolPrinter.typeString(short)
              )
            typeNameEdit :: imports
          } else Nil
        case _ => Nil
      }
    }

    path.headOption match {
      /* `val a = 1` or `var b = 2`
       *     turns into
       * `val a: Int = 1` or `var b: Int = 2`
       *
       *`.map(a => a + a)`
       *     turns into
       * `.map((a: Int) => a + a)`
       */
      case Some(vl @ ValDef(sym, tpt, _)) =>
        val isParam = path.tail.headOption.exists(_.symbol.isAnonymousFunction)
        def baseEdit(withParens: Boolean) = {
          val nameEnd =
            if (isParam) vl.endPos
            else
              tpt.endPos
          val short = shortType(tpt.tpe, history)
          new TextEdit(
            nameEnd.toLSP,
            ": " + symbolPrinter.typeString(short) + {
              if (withParens) ")" else ""
            }
          )
        }

        def typeNameEdit: List[TextEdit] = {
          path match {
            // lambda `{a => ??? }` block and `map{a => ??? }` apply
            case _ :: _ :: (block: untpd.Block) :: (appl: untpd.Apply) :: _
                if isParam =>
              val isParensFunction =
                params.text.toString()(appl.fun.endPos.end) == '('
              val alreadyHasParens =
                params.text.toString()(block.startPos.start) == '('
              if (isParensFunction && !alreadyHasParens)
                new TextEdit(block.startPos.toLSP, "(") :: baseEdit(withParens =
                  true
                ) :: Nil
              else
                baseEdit(withParens = false) :: Nil

            case _ => baseEdit(withParens = false) :: Nil
          }

        }
        def simpleType = {
          val imports =
            autoImportsGen.forSymbol(tpt.tpe.typeSymbol).getOrElse(Nil)
          typeNameEdit ::: imports
        }

        def tupleType(applied: AppliedType) = {
          import scala.meta._
          val pattern =
            params.text.substring(vl.startPos.start, tpt.startPos.start)

          dialects.Scala3("val " + pattern + "???").parse[Source] match {
            case Parsed.Success(Source(List(valDef: m.Defn.Val))) =>
              findTuplePart(
                applied,
                valDef.pats.head,
                vl.startPos.start
              )
            case _ => simpleType
          }
        }
        tpt.tpe match {
          case applied: AppliedType =>
            tupleType(applied)
          case _ =>
            simpleType
        }

      /* `def a[T](param : Int) = param`
       *     turns into
       * `def a[T](param : Int): Int = param`
       */
      case Some(DefDef(name, _, tpt, rhs)) =>
        val short = shortType(tpt.tpe, history)
        val typeNameEdit =
          new TextEdit(
            tpt.endPos.toLSP,
            ": " + symbolPrinter.typeString(short)
          )
        val imports =
          autoImportsGen.forSymbol(tpt.tpe.typeSymbol).getOrElse(Nil)
        typeNameEdit :: imports

      /* `case t =>`
       *  turns into
       * `case t: Int =>`
       */
      case Some(bind @ Bind(name, body)) =>
        val short = shortType(body.tpe, history)

        def baseEdit(withParens: Boolean) = {
          val short = shortType(body.tpe, history)

          new TextEdit(
            bind.endPos.toLSP,
            ": " + symbolPrinter.typeString(short) + {
              if (withParens) ")" else ""
            }
          )
        }
        val imports =
          autoImportsGen.forSymbol(body.tpe.typeSymbol).getOrElse(Nil)
        val typeNameEdit = path match {
          /* In case it's an infix pattern match
           * we need to add () for example in:
           * case (head : Int) :: tail =>
           */
          case _ :: (unappl @ UnApply(_, _, patterns)) :: _
              if patterns.size > 1 =>
            import scala.meta._
            val firstEnd = patterns(0).endPos.end
            val secondStart = patterns(1).startPos.start
            val hasDot = params
              .text()
              .substring(firstEnd, secondStart)
              .tokenize
              .toOption
              .exists(_.tokens.exists(_.is[T.Comma]))
            if (!hasDot) {
              val leftParen = new TextEdit(body.startPos.toLSP, "(")
              leftParen :: baseEdit(withParens = true) :: Nil
            } else baseEdit(withParens = false) :: Nil

          case _ =>
            baseEdit(withParens = false) :: Nil
        }
        typeNameEdit ::: imports

      /* `for(t <- 0 to 10)`
       *  turns into
       * `for(t: Int <- 0 to 10)`
       */
      case Some(i @ Ident(name)) =>
        val short = shortType(i.tpe, history)
        val typeNameEdit = new TextEdit(
          i.endPos.toLSP,
          ": " + symbolPrinter.typeString(short.widen)
        )

        val imports =
          autoImportsGen.forSymbol(i.tpe.typeSymbol).getOrElse(Nil)
        typeNameEdit :: imports

      case _ =>
        Nil
    }

  }

}
