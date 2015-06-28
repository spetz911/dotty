package dotty.tools.dotc
package typer

import dotty.tools.dotc.ast.{ Trees, tpd }
import core._
import Types._, Contexts._, Flags._, Symbols._, Annotations._, Trees._, NameOps._
import Decorators._
import Variances._
import config.Printers.variances

/** Provides `check` method to check that all top-level definitions
 *  in tree are variance correct. Does not recurse inside methods.
 *  The method should be invoked once for each Template.
 */
object VarianceChecker {
  private case class VarianceError(tvar: Symbol, required: Variance)
  def check(tree: tpd.Tree)(implicit ctx: Context) =
    new VarianceChecker()(ctx).Traverser.traverse(tree)
}

class VarianceChecker()(implicit ctx: Context) {
  import VarianceChecker._
  import tpd._

  private object Validator extends TypeAccumulator[Option[VarianceError]] {
    private var base: Symbol = _

    /** Is no variance checking needed within definition of `base`? */
    def ignoreVarianceIn(base: Symbol): Boolean = (
         base.isTerm
      || base.is(Package)
      || base.is(Local)
    )

    /** The variance of a symbol occurrence of `tvar` seen at the level of the definition of `base`.
     *  The search proceeds from `base` to the owner of `tvar`.
     *  Initially the state is covariant, but it might change along the search.
     */
    def relativeVariance(tvar: Symbol, base: Symbol, v: Variance = Covariant): Variance = /*ctx.traceIndented(i"relative variance of $tvar wrt $base, so far: $v")*/ {
      if (base == tvar.owner) v
      else if ((base is Param) && base.owner.isTerm)
        relativeVariance(tvar, paramOuter(base.owner), flip(v))
      else if (ignoreVarianceIn(base.owner)) Bivariant
      else if (base.isAliasType) relativeVariance(tvar, base.owner, Invariant)
      else relativeVariance(tvar, base.owner, v)
    }

    /** The next level to take into account when determining the
     *  relative variance with a method parameter as base. The method
     *  is always skipped. If the method is a constructor, we also skip
     *  its class owner, because constructors are not checked for variance
     *  relative to the type parameters of their own class. On the other
     *  hand constructors do count for checking the variance of type parameters
     *  of enclosing classes. I believe the Scala 2 rules are too lenient in
     *  that respect.
     */
    private def paramOuter(meth: Symbol) =
      if (meth.isConstructor) meth.owner.owner else meth.owner

    /** Check variance of abstract type `tvar` when referred from `base`. */
    private def checkVarianceOfSymbol(tvar: Symbol): Option[VarianceError] = {
      val relative = relativeVariance(tvar, base)
      if (relative == Bivariant) None
      else {
        val required = compose(relative, this.variance)
        def tvar_s = s"$tvar (${varianceString(tvar.flags)} ${tvar.showLocated})"
        def base_s = s"$base in ${base.owner}" + (if (base.owner.isClass) "" else " in " + base.owner.enclosingClass)
        ctx.log(s"verifying $tvar_s is ${varianceString(required)} at $base_s")
        ctx.log(s"relative variance: ${varianceString(relative)}")
        ctx.log(s"current variance: ${this.variance}")
        ctx.log(s"owner chain: ${base.ownersIterator.toList}")
        if (tvar is required) None
        else Some(VarianceError(tvar, required))
      }
    }

    /** For PolyTypes, type parameters are skipped because they are defined
     *  explicitly (their TypeDefs will be passed here.) For MethodTypes, the
     *  same is true of the parameters (ValDefs).
     */
    def apply(status: Option[VarianceError], tp: Type): Option[VarianceError] = ctx.traceIndented(s"variance checking $tp of $base at $variance", variances) {
      if (status.isDefined) status
      else tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.variance != 0 && base.isContainedIn(sym.owner)) checkVarianceOfSymbol(sym)
          else if (sym.isAliasType) this(status, sym.info)
          else foldOver(status, tp)
        case tp: MethodType =>
          this(status, tp.resultType) // params will be checked in their TypeDef nodes.
        case tp: PolyType =>
          this(status, tp.resultType) // params will be checked in their ValDef nodes.
        case AnnotatedType(annot, _) if annot.symbol == defn.UncheckedVarianceAnnot =>
          status
        //case tp: ClassInfo =>
        //  ???  not clear what to do here yet. presumably, it's all checked at local typedefs
        case _ =>
          foldOver(status, tp)
      }
    }

    def validateDefinition(base: Symbol): Option[VarianceError] = {
      val saved = this.base
      this.base = base
      try apply(None, base.info)
      finally this.base = saved
    }
  }

  private object Traverser extends TreeTraverser {
    def checkVariance(sym: Symbol) = Validator.validateDefinition(sym) match {
      case Some(VarianceError(tvar, required)) =>
        ctx.error(
          i"${varianceString(tvar.flags)} $tvar occurs in ${varianceString(required)} position in type ${sym.info} of $sym",
          sym.pos)
      case None =>
    }

    override def traverse(tree: Tree)(implicit ctx: Context) = {
      def sym = tree.symbol
      // No variance check for private/protected[this] methods/values.
      def skip = !sym.exists || sym.is(Local)
      tree match {
        case defn: MemberDef if skip =>
          ctx.debuglog(s"Skipping variance check of ${sym.showDcl}")
        case tree: TypeDef =>
          checkVariance(sym)
          traverseChildren(tree)
        case tree: ValDef =>
          checkVariance(sym)
        case DefDef(_, tparams, vparamss, _, _) =>
          checkVariance(sym)
          tparams foreach traverse
          vparamss foreach (_ foreach traverse)
        case Template(_, _, _, body) =>
          traverseChildren(tree)
        case _ =>
      }
    }
  }
}
