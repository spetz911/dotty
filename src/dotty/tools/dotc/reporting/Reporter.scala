package dotty.tools
package dotc
package reporting

import core.Contexts._
import util.{SourcePosition, NoSourcePosition}
import util.{SourceFile, NoSource}
import core.Decorators.PhaseListDecorator
import collection.mutable
import config.Settings.Setting
import config.Printers
import java.lang.System.currentTimeMillis
import typer.ErrorReporting.DiagnosticString
import typer.Mode

object Reporter {

  private val ERROR = 2
  private val WARNING = 1
  private val INFO = 0

  class Diagnostic(msgFn: => String, val pos: SourcePosition, val level: Int) extends Exception {
    import DiagnosticString._

    private var myMsg: String = null
    private var myIsNonSensical: Boolean = false

    /** The message to report */
    def msg: String = {
      if (myMsg == null) {
        myMsg = msgFn
        if (myMsg.contains(nonSensicalStartTag)) {
          myIsNonSensical = true
          // myMsg might be composed of several d"..." invocations -> nested nonsensical tags possible
          myMsg = myMsg.replaceAllLiterally(nonSensicalStartTag, "").replaceAllLiterally(nonSensicalEndTag, "")
        }
      }
      myMsg
    }

    /** Report in current reporter */
    def report(implicit ctx: Context) = ctx.reporter.report(this)

    def isNonSensical = { msg; myIsNonSensical }
    def isSuppressed(implicit ctx: Context): Boolean = !ctx.settings.YshowSuppressedErrors.value && isNonSensical

    override def toString = s"$getClass at $pos: $msg"
    override def getMessage() = msg

    def checkingStr: String = msgFn
  }

  class Error(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, ERROR)
  class Warning(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, WARNING)
  class Info(msgFn: => String, pos: SourcePosition) extends Diagnostic(msgFn, pos, INFO)

  abstract class ConditionalWarning(msgFn: => String, pos: SourcePosition) extends Warning(msgFn, pos) {
    def enablingOption(implicit ctx: Context): Setting[Boolean]
  }
  class FeatureWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.feature
  }
  class UncheckedWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.unchecked
  }
  class DeprecationWarning(msgFn: => String, pos: SourcePosition) extends ConditionalWarning(msgFn, pos) {
    def enablingOption(implicit ctx: Context) = ctx.settings.deprecation
  }
}

import Reporter._

trait Reporting { this: Context =>

  /** For sending messages that are printed only if -verbose is set */
  def inform(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.verbose.value) this.println(msg, pos)

  def println(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Info(msg, pos))

  def deprecationWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new DeprecationWarning(msg, pos))

  def uncheckedWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new UncheckedWarning(msg, pos))

  def featureWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new FeatureWarning(msg, pos))

  def warning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    reporter.report(new Warning(msg, pos))

  def strictWarning(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.strict.value) error(msg, pos)
    else warning(msg + "\n(This would be an error under strict mode)", pos)

  def error(msg: => String, pos: SourcePosition = NoSourcePosition): Unit = {
    // println("*** ERROR: " + msg) // !!! DEBUG
    reporter.report(new Error(msg, pos))
  }

  def restrictionError(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    error(s"Implementation restriction: $msg", pos)

  def incompleteInputError(msg: String, pos: SourcePosition = NoSourcePosition)(implicit ctx: Context): Unit =
    reporter.incomplete(new Error(msg, pos))(ctx)

  /** Log msg if settings.log contains the current phase.
   *  See [[config.CompilerCommand#explainAdvanced]] for the exact meaning of
   *  "contains" here.
   */
  def log(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.log.value.containsPhase(phase))
      this.println(s"[log ${ctx.phasesStack.reverse.mkString(" -> ")}] $msg", pos)

  def debuglog(msg: => String): Unit =
    if (ctx.debug) log(msg)

  def informTime(msg: => String, start: Long): Unit = {
    def elapsed = s" in ${currentTimeMillis - start}ms"
    informProgress(msg + elapsed)
  }

  def informProgress(msg: => String) =
    inform("[" + msg + "]")

  def trace[T](msg: => String)(value: T) = {
    log(msg + " " + value)
    value
  }

  def debugwarn(msg: => String, pos: SourcePosition = NoSourcePosition): Unit =
    if (this.settings.debug.value) warning(msg, pos)

  def debugTraceIndented[T](question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T =
    conditionalTraceIndented(this.settings.debugTrace.value, question, printer, show)(op)

  def conditionalTraceIndented[T](cond: Boolean, question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T =
    if (cond) traceIndented(question, printer, show)(op)
    else op

  def traceIndented[T](question: => String, printer: Printers.Printer = Printers.default, show: Boolean = false)(op: => T): T = {
    def resStr(res: Any): String = res match {
      case res: printing.Showable if show => res.show
      case _ => String.valueOf(res)
    }
    if (printer eq config.Printers.noPrinter) op
    else traceIndented[T](s"==> $question?", (res: Any) => s"<== $question = ${resStr(res)}")(op)
  }

  def traceIndented[T](leading: => String, trailing: Any => String)(op: => T): T = {
    var finalized = false
    var logctx = this
    while (logctx.reporter.isInstanceOf[StoreReporter]) logctx = logctx.outer
    def finalize(result: Any, note: String) =
      if (!finalized) {
        base.indent -= 1
        logctx.log(s"${base.indentTab * base.indent}${trailing(result)}$note")
        finalized = true
      }
    try {
      logctx.log(s"${base.indentTab * base.indent}$leading")
      base.indent += 1
      val res = op
      finalize(res, "")
      res
    } catch {
      case ex: Throwable =>
        finalize("<missing>", s" (with exception $ex)")
        throw ex
    }
  }

  def errorsReported: Boolean = outersIterator exists (_.reporter.hasErrors)
}

/**
 * This interface provides methods to issue information, warning and
 * error messages.
 */
abstract class Reporter {

  /** Report a diagnostic */
  protected def doReport(d: Diagnostic)(implicit ctx: Context): Unit

 /** Whether very long lines can be truncated.  This exists so important
   *  debugging information (like printing the classpath) is not rendered
   *  invisible due to the max message length.
   */
  private var _truncationOK: Boolean = true
  def truncationOK = _truncationOK
  def withoutTruncating[T](body: => T): T = {
    val saved = _truncationOK
    _truncationOK = false
    try body
    finally _truncationOK = saved
  }

  type ErrorHandler = Diagnostic => Context => Unit
  private var incompleteHandler: ErrorHandler = d => c => report(d)(c)
  def withIncompleteHandler[T](handler: ErrorHandler)(op: => T): T = {
    val saved = incompleteHandler
    incompleteHandler = handler
    try op
    finally incompleteHandler = saved
  }

  var errorCount = 0
  var warningCount = 0
  def hasErrors = errorCount > 0
  def hasWarnings = warningCount > 0

  val unreportedWarnings = new mutable.HashMap[String, Int] {
    override def default(key: String) = 0
  }

  def report(d: Diagnostic)(implicit ctx: Context): Unit = if (!isHidden(d)) {
    doReport(d)(ctx.addMode(Mode.Printing))
    d match {
      case d: ConditionalWarning if !d.enablingOption.value => unreportedWarnings(d.enablingOption.name) += 1
      case d: Warning => warningCount += 1
      case d: Error => errorCount += 1
      case d: Info => // nothing to do here
      // match error if d is something else
    }
  }

  def incomplete(d: Diagnostic)(implicit ctx: Context): Unit =
    incompleteHandler(d)(ctx)


  /** Print a summary */
  def printSummary(implicit ctx: Context): Unit = {
    if (warningCount > 0) ctx.println(countString(warningCount, "warning") + " found")
    if (errorCount > 0) ctx.println(countString(errorCount, "error") + " found")
    for ((settingName, count) <- unreportedWarnings)
      ctx.println(s"there were $count ${settingName.tail} warning(s); re-run with $settingName for details")
  }

  /** Returns a string meaning "n elements". */
  private def countString(n: Int, elements: String): String = n match {
    case 0 => "no " + elements + "s"
    case 1 => "one " + elements
    case 2 => "two " + elements + "s"
    case 3 => "three " + elements + "s"
    case 4 => "four " + elements + "s"
    case _ => n + " " + elements + "s"
  }

  /** Should this diagnostic not be reported at all? */
  def isHidden(d: Diagnostic)(implicit ctx: Context): Boolean = ctx.mode.is(Mode.Printing)

  /** Does this reporter contain not yet reported errors or warnings? */
  def hasPending: Boolean = false

  /** Issue all error messages in this reporter to next outer one, or make sure they are written. */
  def flush()(implicit ctx: Context): Unit = {}
}
