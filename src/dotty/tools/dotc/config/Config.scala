package dotty.tools.dotc.config

object Config {

  final val cacheMembersNamed = true
  final val cacheAsSeenFrom = true
  final val useFingerPrints = true
  final val cacheMemberNames = true
  final val cacheImplicitScopes = true

  final val checkCacheMembersNamed = false

  /** When updating a constraint bound, check that the constrained parameter
   *  does not appear at the top-level of either of its bounds.
   */
  final val checkConstraintsNonCyclic = false

  /** Like `checkConstraintsNonCyclic`, but all constrained parameters
   *  are tested for direct or indirect dependencies, each time a
   *  constraint is added in TypeComparer.
   */
  final val checkConstraintsNonCyclicTrans = false

  /** Check that each constraint resulting from a subtype test
   *  is satisfiable.
   */
  final val checkConstraintsSatisfiable = false

  /** Check that each constraint is fully propagated. i.e.
   *  If P <: Q then the upper bound of P is a subtype of the upper bound of Q
   *  and the lower bound of Q is a subtype of the lower bound of P.
   */
  final val checkConstraintsPropagated = false

  /** Check that constraints of globally committable typer states are closed.
   *  NOTE: When enabled, the check can cause CyclicReference errors because
   *  it traverses all elements of a type. Such failures were observed when
   *  compiling all of dotty together (source seems to be in GenBCode which
   *  accesses javac's settings.)
   *
   *  It is recommended to turn this option on only when chasing down
   *  a PolyParam instantiation error. See comment in Types.TypeVar.instantiate.
   */
  final val debugCheckConstraintsClosed = false

  /** Check that no type appearing as the info of a SymDenotation contains
   *  skolem types.
   */
  final val checkNoSkolemsInInfo = false

  /** Type comparer will fail with an assert if the upper bound
   *  of a constrained parameter becomes Nothing. This should be turned
   *  on only for specific debugging as normally instantiation to Nothing
   *  is not an error consdition.
   */
  final val failOnInstantiationToNothing = false

  /** Enable noDoubleDef checking if option "-YnoDoubleDefs" is set.
    * The reason to have an option as well as the present global switch is
    * that the noDoubleDef checking is done in a hotspot, and we do not
    * want to incur the overhead of checking an option each time.
    */
  final val checkNoDoubleBindings = true

  /** Show subtype traces for all deep subtype recursions */
  final val traceDeepSubTypeRecursions = false

  /** When explaining subtypes and this flag is set, also show the classes of the compared types. */
  final val verboseExplainSubtype = true

  /** If this flag is set, take the fast path when comparing same-named type-aliases and types */
  final val fastPathForRefinedSubtype = true

  /** When set, use new signature-based matching.
   *  Advantage of doing so: It's supposed to be faster
   *  Disadvantage: It might hide inconsistencies, so while debugging it's better to turn it off
   */
  final val newMatch = false

  /** The recursion depth for showing a summarized string */
  final val summarizeDepth = 2

  /** Check that variances of lambda arguments match the
   *  variance of the underlying lambda class.
   */
  final val checkLambdaVariance = false

  /** Check that certain types cannot be created in erasedTypes phases */
  final val checkUnerased = true


  /** Initial size of superId table */
  final val InitialSuperIdsSize = 4096

  /** Initial capacity of uniques HashMap */
  final val initialUniquesCapacity = 40000

  /** How many recursive calls to NamedType#underlying are performed before logging starts. */
  final val LogPendingUnderlyingThreshold = 50

  /** How many recursive calls to isSubType are performed before logging starts. */
  final val LogPendingSubTypesThreshold = 50

  /** How many recursive calls to findMember are performed before logging names starts
   *  Note: this threshold has to be chosen carefully. Too large, and programs
   *  like tests/pos/IterableSelfRec go into polynomial (or even exponential?)
   *  compile time slowdown. Too small and normal programs will cause the compiler  to
   *  do inefficient operations on findMember. The current value is determined
   *  so that (1) IterableSelfRec still compiles in reasonable time (< 10sec) (2) Compiling
   *  dotty itself only causes small pending names lists to be generated (we measured
   *  at max 6 elements) and these lists are never searched with contains.
   */
  final val LogPendingFindMemberThreshold = 10

  /** Maximal number of outstanding recursive calls to findMember  */
  final val PendingFindMemberLimit = LogPendingFindMemberThreshold * 4
}
