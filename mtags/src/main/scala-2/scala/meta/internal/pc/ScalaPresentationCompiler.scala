package scala.meta.internal.pc

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util
import java.util.Optional
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.ScheduledExecutorService
import java.util.logging.Logger
import java.{util => ju}

import scala.collection.Seq
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.StoreReporter

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.mtags.BuildInfo
import scala.meta.pc.AutoImportsResult
import scala.meta.pc.DefinitionResult
import scala.meta.pc.OffsetParams
import scala.meta.pc.PresentationCompiler
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch
import scala.meta.pc.VirtualFileParams

import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionList
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.Hover
import org.eclipse.lsp4j.SelectionRange
import org.eclipse.lsp4j.SignatureHelp
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j.SignatureHelpTriggerKind

case class ScalaPresentationCompiler(
    buildTargetIdentifier: String = "",
    classpath: Seq[Path] = Nil,
    options: List[String] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    ec: ExecutionContextExecutor = ExecutionContext.global,
    sh: Option[ScheduledExecutorService] = None,
    config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    workspace: Option[Path] = None
) extends PresentationCompiler {
  implicit val executionContext: ExecutionContextExecutor = ec

  val scalaVersion = BuildInfo.scalaCompilerVersion

  val logger: Logger =
    Logger.getLogger(classOf[ScalaPresentationCompiler].getName)

  override def withSearch(search: SymbolSearch): PresentationCompiler =
    copy(search = search)

  override def withWorkspace(workspace: Path): PresentationCompiler =
    copy(workspace = Some(workspace))

  override def withExecutorService(
      executorService: ExecutorService
  ): PresentationCompiler =
    copy(ec = ExecutionContext.fromExecutorService(executorService))

  override def withScheduledExecutorService(
      sh: ScheduledExecutorService
  ): PresentationCompiler =
    copy(sh = Some(sh))

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): PresentationCompiler =
    copy(config = config)

  def this() = this(buildTargetIdentifier = "")

  val compilerAccess =
    new ScalaCompilerAccess(
      config,
      sh,
      () => new ScalaCompilerWrapper(newCompiler())
    )(
      ec
    )

  override def shutdown(): Unit = {
    compilerAccess.shutdown()
  }

  override def restart(): Unit = {
    compilerAccess.shutdownCurrentCompiler()
  }

  def isLoaded(): Boolean = compilerAccess.isLoaded()

  override def newInstance(
      buildTargetIdentifier: String,
      classpath: util.List[Path],
      options: util.List[String]
  ): PresentationCompiler = {
    copy(
      buildTargetIdentifier = buildTargetIdentifier,
      classpath = classpath.asScala,
      options = options.asScala.toList
    )
  }

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[ju.List[Diagnostic]] = {
    CompletableFuture.completedFuture(Nil.asJava)
  }

  def didClose(uri: URI): Unit = {}

  /// under development s.sasaki ↓
  // { line: 2, startChar: 10, length: 4, tokenType: "type", tokenModifiers: [] },
  def semanticTokens(
      params: VirtualFileParams
   ): CompletableFuture[ju.List[String]] = {
  //  ): CompletableFuture[ju.List[TextEdit]] = {
    import scala.collection.mutable.ListBuffer 
    import scala.tools.nsc.ast.parser.Tokens

    logger.info("Debug : Scala-PC :")

    // val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    val empty: ju.List[String] = new ju.ArrayList[String]()

    logger.info(" --Before compilerAccess---")
// try {
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      logger.info(" --getting from pc---")
      val scanner = pc.compiler()
        .newUnitParser(params.text()).newScanner()
      scanner.init()
      val tokenBuffer = ListBuffer.empty[String]
      // val buffer = ListBuffer.empty[Integer]
      var line = 1
      var lastOffset = 0
      while (scanner.token != Tokens.EOF) {
        val token = scanner.token
        token match {
          case Tokens.NEWLINE | Tokens.NEWLINES =>
            line += 1
            lastOffset = scanner.offset
          // SemanticTokenTypes.Keyword -> 1
          case _ =>
            tokenBuffer.addAll(List(
              token.toString()
            ,  line.toString()
            ,  (scanner.offset - lastOffset).toString // start offset
            ,  scanner.name.toString //Tername
            ,  scanner.strVal
            ,  scanner.base.toString()
            ))
logger.info("token : " + token.toString())
logger.info("line : " +  line.toString())
logger.info("start offset : " +  (scanner.offset - lastOffset).toString) // start offset
logger.info("termname : "  + scanner.name.toString) //Tername
logger.info("strVal : " +  scanner.strVal)
logger.info("base : " +  scanner.base.toString())
            //Tokens.DEF
            // convert offset to line and character
            // scanner.offset -> (line, character)
            // length is 3, type is 1, no modifiers currently
            // val character = scanner.offset - lastOffset
            // buffer.addAll(List(line, character, 3, 1, 0))
        }
        // val o = scanner.offset
        scanner.nextToken()

      }
      logger.info(" --compiler process end---")
      // new SemanticTokens(buffer.toList.asJava)
      tokenBuffer.toList.asJava
      Nil.asJava
    }
// }catch{
//     case e:Exception => logger.info(e.toString())
//     new CompletableFuture[ju.List[String]]()
// }
  }
  /// under development ↑
  override def complete(
      params: OffsetParams
  ): CompletableFuture[CompletionList] =
    compilerAccess.withInterruptableCompiler(
      EmptyCompletionList(),
      params.token
    ) { pc => new CompletionProvider(pc.compiler(), params).completions() }

  override def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      new CompletionProvider(pc.compiler(), params).implementAll()
    }
  }

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      new InferredTypeProvider(pc.compiler(), params).inferredTypeEdits().asJava
    }
  }

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: ju.List[Integer]
  ): CompletableFuture[ju.List[TextEdit]] = {
    val empty: ju.List[TextEdit] = new ju.ArrayList[TextEdit]()
    compilerAccess.withInterruptableCompiler(empty, params.token) { pc =>
      new ConvertToNamedArgumentsProvider(
        pc.compiler(),
        params,
        argIndices.asScala.map(_.toInt).toSet
      ).convertToNamedArguments.asJava
    }
  }

  override def autoImports(
      name: String,
      params: OffsetParams
  ): CompletableFuture[ju.List[AutoImportsResult]] =
    compilerAccess.withInterruptableCompiler(
      List.empty[AutoImportsResult].asJava,
      params.token
    ) { pc =>
      new AutoImportsProvider(pc.compiler(), name, params).autoImports().asJava
    }

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] =
    CompletableFuture.completedFuture("")

  // NOTE(olafur): hover and signature help use a "shared" compiler instance because
  // we don't typecheck any sources, we only poke into the symbol table.
  // If we used a shared compiler then we risk hitting `Thread.interrupt`,
  // which can close open `*-sources.jar` files containing Scaladoc/Javadoc strings.
  override def completionItemResolve(
      item: CompletionItem,
      symbol: String
  ): CompletableFuture[CompletionItem] =
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(item) { pc =>
        new CompletionItemResolver(pc.compiler()).resolve(item, symbol)
      }
    }

  override def signatureHelp(
      params: OffsetParams
  ): CompletableFuture[SignatureHelp] =
    compilerAccess.withNonInterruptableCompiler(
      new SignatureHelp(),
      params.token
    ) { pc => new SignatureHelpProvider(pc.compiler()).signatureHelp(params) }

  override def hover(
      params: OffsetParams
  ): CompletableFuture[Optional[Hover]] =
    compilerAccess.withNonInterruptableCompiler(
      Optional.empty[Hover](),
      params.token
    ) { pc =>
      Optional.ofNullable(
        new HoverProvider(pc.compiler(), params).hover().orNull
      )
    }

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] = {
    compilerAccess.withNonInterruptableCompiler(
      DefinitionResultImpl.empty,
      params.token
    ) { pc => new PcDefinitionProvider(pc.compiler(), params).definition() }
  }

  override def semanticdbTextDocument(
      uri: URI,
      code: String
  ): CompletableFuture[Array[Byte]] = {
    compilerAccess.withInterruptableCompiler(
      Array.emptyByteArray,
      EmptyCancelToken
    ) { pc =>
      new SemanticdbTextDocumentProvider(
        pc.compiler(),
        config.semanticdbCompilerOptions().asScala.toList
      )
        .textDocument(uri, code)
        .toByteArray
    }
  }

  override def selectionRange(
      params: ju.List[OffsetParams]
  ): CompletableFuture[ju.List[SelectionRange]] = {
    CompletableFuture.completedFuture {
      compilerAccess.withSharedCompiler(List.empty[SelectionRange].asJava) {
        pc =>
          new SelectionRangeProvider(pc.compiler(), params)
            .selectionRange()
            .asJava
      }
    }
  }

  def newCompiler(): MetalsGlobal = {
    val classpath = this.classpath.mkString(File.pathSeparator)
    val vd = new VirtualDirectory("(memory)", None)
    val settings = new Settings
    settings.Ymacroexpand.value = "discard"
    settings.outputDirs.setSingleOutput(vd)
    settings.classpath.value = classpath
    settings.YpresentationAnyThread.value = true
    if (
      !BuildInfo.scalaCompilerVersion.startsWith("2.11") &&
      BuildInfo.scalaCompilerVersion != "2.12.4"
    ) {
      settings.processArguments(
        List("-Ycache-plugin-class-loader:last-modified"),
        processAll = true
      )
    }
    if (classpath.isEmpty) {
      settings.usejavacp.value = true
    }
    val (isSuccess, unprocessed) =
      settings.processArguments(options, processAll = true)
    if (unprocessed.nonEmpty || !isSuccess) {
      logger.warning(s"Unknown compiler options: ${unprocessed.mkString(", ")}")
    }
    new MetalsGlobal(
      settings,
      new StoreReporter,
      search,
      buildTargetIdentifier,
      config,
      workspace
    )
  }

  // ================
  // Internal methods
  // ================

  override def diagnosticsForDebuggingPurposes(): util.List[String] = {
    compilerAccess.reporter.infos.iterator
      .map { info =>
        new StringBuilder()
          .append(info.pos.source.file.path)
          .append(":")
          .append(info.pos.column)
          .append(" ")
          .append(info.msg)
          .append("\n")
          .append(info.pos.lineContent)
          .append("\n")
          .append(info.pos.lineCaret)
          .toString
      }
      .filterNot(_.contains("_CURSOR_"))
      .toList
      .asJava
  }
}
