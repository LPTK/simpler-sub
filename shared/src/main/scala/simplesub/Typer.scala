package simplesub

import scala.collection.mutable
import scala.collection.mutable.{Map => MutMap, Set => MutSet}
import scala.collection.immutable.{SortedSet, SortedMap}
import scala.util.chaining._
import scala.annotation.tailrec

final case class TypeError(val msg: String) extends Exception(msg)

/** A class encapsulating type inference state.
 *  It uses its own internal representation of types and type variables, using mutable data structures.
 *  Inferred SimpleType values are then turned into CompactType values for simplification (see TypeSimplifier.scala).
 *  In order to turn the resulting CompactType into a simplesub.Type, we use `coalesceCompactType`.
 */
class Typer(protected val dbg: Boolean) extends TyperDebugging {
  
  type Ctx = Map[String, TypeScheme]
  
  val BoolType: Primitive = Primitive("bool")
  val IntType: Primitive = Primitive("int")
  
  val builtins: Ctx = Map(
    "true" -> BoolType,
    "false" -> BoolType,
    "not" -> Function(BoolType, BoolType),
    "succ" -> Function(IntType, IntType),
    "add" -> Function(IntType, Function(IntType, IntType)),
    "if" -> {
      val v = freshVar
      PolymorphicType(Function(BoolType, Function(v, Function(v, v))))
    }
  )
  private val builtinsSize = builtins.size
  
  /** The main type inference function */
  def inferTypes(pgrm: Pgrm, ctx: Ctx = builtins): List[Either[TypeError, PolymorphicType]] =
    pgrm.defs match {
      case (isrec, nme, rhs) :: defs =>
        val ty_sch = try Right(typeLetRhs(isrec, nme, rhs)(ctx)) catch {
          case err: TypeError => Left(err) }
        ty_sch :: inferTypes(Pgrm(defs), ctx + (nme -> ty_sch.getOrElse(freshVar)))
      case Nil => Nil
    }
  
  def inferType(term: Term, ctx: Ctx = builtins): SimpleType = typeTerm(term)(ctx)
  
  /** Infer the type of a let binding right-hand side. */
  def typeLetRhs(isrec: Boolean, nme: String, rhs: Term)(implicit ctx: Ctx): PolymorphicType = {
    val res = if (isrec) {
      val e_ty = freshVar
      val ty = typeTerm(rhs)(ctx + (nme -> e_ty))
      constrain(ty, e_ty)
      e_ty
    } else typeTerm(rhs)(ctx)
    PolymorphicType(res)
  }
  
  /** Infer the type of a term. */
  def typeTerm(term: Term)(implicit ctx: Ctx): SimpleType = {
    lazy val res = freshVar
    term match {
      case Var(name) =>
        ctx.getOrElse(name, err("identifier not found: " + name)).instantiate
      case Lam(name, body) =>
        val param = freshVar
        val body_ty = typeTerm(body)(ctx + (name -> param))
        Function(param, body_ty)
      case App(f, a) =>
        val f_ty = typeTerm(f)
        val a_ty = typeTerm(a)
        constrain(f_ty, Function(a_ty, res))
        res
      case Lit(n) =>
        IntType
      case Sel(obj, name) =>
        val obj_ty = typeTerm(obj)
        constrain(obj_ty, Record((name, res) :: Nil))
        res
      case Rcd(fs) =>
        Record(fs.map { case (n, t) => (n, typeTerm(t)) })
      case Let(isrec, nme, rhs, bod) =>
        if (isrec) if (ctx.sizeCompare(builtinsSize) <= 0) {
          val n_ty = typeLetRhs(isrec, nme, rhs)
          typeTerm(bod)(ctx + (nme -> n_ty))
        } else err("Unsupported: local recursive let binding")
        else typeTerm(App(Lam(nme, bod), rhs))
    }
  }
  
  /** Constrains the types to enforce a subtyping relationship `lhs` <: `rhs`. */
  def constrain(lhs: SimpleType, rhs: SimpleType): Unit = {
    // println(s"Constr. $lhs <: $rhs")
    if (lhs is rhs) return ()
    (lhs, rhs) match {
      case (Bot, _) | (_, Top) => ()
      case (Function(l0, r0), Function(l1, r1)) =>
        constrain(l1, l0)
        constrain(r0, r1)
      case (Record(fs0), Record(fs1)) =>
        fs1.foreach { case (n1, t1) =>
          fs0.find(_._1 === n1).fold(
            err(s"missing field: $n1 in ${lhs.show}")
          ) { case (n0, t0) => constrain(t0, t1) }
        }
      case (lhs: Variable, rhs: Variable) =>
        lhs.unifyWith(rhs)
      case (lhs: Variable, rhs: ConcreteType) =>
        lhs.newUpperBound(rhs)
      case (lhs: ConcreteType, rhs: Variable) =>
        rhs.newLowerBound(lhs)
      case _ => err(s"cannot constrain ${lhs.show} <: ${rhs.show}")
    }
  }
  
  def err(msg: String): Nothing = throw TypeError(msg)
  
  private var freshCount = 0
  def freshVar: Variable = new Variable(Bot, Top)
  
  def freshenType(ty: SimpleType): SimpleType = {
    val freshened = MutMap.empty[Variable, Variable]
    def freshen(ty: SimpleType): SimpleType = ty match {
      case tv0: Variable =>
        val tv = tv0.representative
        freshened.get(tv) match {
          case Some(tv) => tv
          case None =>
            val v = tv.makeCopy
            freshened += tv -> v
            v
        }
      case Function(l, r) => Function(freshen(l), freshen(r))
      case Record(fs) => Record(fs.map(ft => ft._1 -> freshen(ft._2)))
      case Primitive(_) | Top | Bot => ty
    }
    freshen(ty)
  }
  
  def glbConcrete(lhs: ConcreteType, rhs: ConcreteType): ConcreteType = (lhs, rhs) match {
    case (Top, _) => rhs
    case (_, Top) => lhs
    case (Bot, _) | (_, Bot) => Bot
    case (Function(l0, r0), Function(l1, r1)) => Function(lub(l0, l1), glb(r0, r1))
    case (Record(fs0), Record(fs1)) => Record(mergeMap(fs0, fs1)(glb(_, _)).toList)
    case (Primitive(n0), Primitive(n1)) if n0 === n1 => Primitive(n0)
    case _ => Bot
  }
  def lubConcrete(lhs: ConcreteType, rhs: ConcreteType): ConcreteType = (lhs, rhs) match {
    case (Bot, _) => rhs
    case (_, Bot) => lhs
    case (Top, _) | (_, Top) => Top
    case (Function(l0, r0), Function(l1, r1)) => Function(glb(l0, l1), lub(r0, r1))
    case (Record(fs0), Record(fs1)) =>
      val fs1m = fs1.toMap
      Record(fs0.flatMap {
        case (n, t0) => fs1m.get(n) match { case Some(t1) => n -> lub(t0, t1) :: Nil; case None => Nil }
      })
    case (Primitive(n0), Primitive(n1)) if n0 === n1 => Primitive(n0)
    case _ => Top
  }
  def glb(lhs: SimpleType, rhs: SimpleType): SimpleType = (lhs, rhs) match {
    case (c0: ConcreteType, c1: ConcreteType) => glbConcrete(c0, c1)
    case (v0: Variable, v1: Variable) => v0.unifyWith(v1); v1
    case (c0: ConcreteType, v1: Variable) => v1.newUpperBound(c0); v1
    case (v0: Variable, c1: ConcreteType) => v0.newUpperBound(c1); v0
  }
  def lub(lhs: SimpleType, rhs: SimpleType): SimpleType = (lhs, rhs) match {
    case (c0: ConcreteType, c1: ConcreteType) => lubConcrete(c0, c1)
    case (v0: Variable, v1: Variable) => v0.unifyWith(v1); v1
    case (c0: ConcreteType, v1: Variable) => v1.newLowerBound(c0); v1
    case (v0: Variable, c1: ConcreteType) => v0.newLowerBound(c1); v0
  }
  
  
  // The data types used for type inference:
  
  /** A type that potentially contains universally quantified type variables,
   *  and which can be isntantiated to a given level. */
  sealed abstract class TypeScheme {
    def instantiate: SimpleType
  }
  /** A type with universally quantified type variables
   *  (by convention, those variables of level greater than `level` are considered quantified). */
  case class PolymorphicType(body: SimpleType) extends TypeScheme {
    def instantiate = freshenType(body)
  }
  /** A type without universally quantified type variables. */
  sealed abstract class SimpleType extends TypeScheme with SimpleTypeImpl {
    def instantiate = this
  }
  
  sealed abstract class ConcreteType extends SimpleType
  
  case object Top extends ConcreteType
  case object Bot extends ConcreteType
  
  case class Function(lhs: SimpleType, rhs: SimpleType) extends ConcreteType {
    override def toString = s"($lhs -> $rhs)"
  }
  case class Record(fields: List[(String, SimpleType)]) extends ConcreteType {
    override def toString = s"{${fields.map(f => s"${f._1}: ${f._2}").mkString(", ")}}"
  }
  case class Primitive(name: String) extends ConcreteType {
    override def toString = name
  }
  
  /** A type variable living at a certain polymorphism level `level`, with mutable bounds.
   *  Invariant: Types appearing in the bounds never have a level higher than this variable's `level`. */
  final class Variable(
      private var _lowerBound: ConcreteType,
      private var _upperBound: ConcreteType,
  ) extends SimpleType {
    private[simplesub] val uid: Int = { freshCount += 1; freshCount - 1 }
    private var _representative: Option[Variable] = None
    def representative: Variable =
      _representative match {
        case Some(v) =>
          val rep = v.representative
          if (rep isnt v) _representative = Some(rep)
          rep
        case None => this
      }
    def lowerBound: ConcreteType = representative._lowerBound
    def upperBound: ConcreteType = representative._upperBound
    def newUpperBound(ub: ConcreteType): Unit = {
      occursCheck(ub, true)
      val rep = representative
      rep._upperBound = glbConcrete(rep._upperBound, ub)
      constrain(rep._lowerBound, ub)
    }
    def newLowerBound(lb: ConcreteType): Unit = {
      occursCheck(lb, false)
      val rep = representative
      rep._lowerBound = lubConcrete(rep._lowerBound, lb)
      constrain(lb, rep._upperBound)
    }
    private def occursCheck(ty: ConcreteType, dir: Boolean): Unit = {
      if (ty.getVars.contains(representative)) {
        val boundsStr = ty.showBounds
        err(s"Illegal cyclic constraint: $this ${if (dir) "<:" else ":>"} $ty"
          + (if (boundsStr.isEmpty) "" else "\n\t\twhere: " + boundsStr))
      }
    }
    def makeCopy: Variable = new Variable(lowerBound, upperBound)
    lazy val asTypeVar = new TypeVariable("α", uid)
    def unifyWith(that: Variable): Unit = {
      val rep0 = representative
      val rep1 = that.representative
      if (rep0 isnt rep1) {
        occursCheck(rep1._lowerBound, false)
        occursCheck(rep1._upperBound, true)
        rep1.newLowerBound(rep0._lowerBound)
        rep1.newUpperBound(rep0._upperBound)
        rep0._representative = Some(rep1)
      }
    }
    override def toString: String =
      // _representative.fold("α" + uid)(_.toString + "<~" + uid)
      "α" + representative.uid
    override def hashCode: Int = uid
  }
  
  
  
  
  def simplifyType(st: SimpleType): SimpleType = {
    
    val pos, neg = mutable.Set.empty[Variable]
    
    def analyze(st: SimpleType, pol: Boolean): Unit = st match {
      case Record(fs) => fs.foreach(f => analyze(f._2, pol))
      case Function(l, r) => analyze(l, !pol); analyze(r, pol)
      case v0: Variable =>
        val v = v0.representative
        (if (pol) pos else neg) += v
        analyze(if (pol) v.lowerBound else v.upperBound, pol)
      case Primitive(_) | Top | Bot => ()
    }
    analyze(st, true)
    
    val mapping = mutable.Map.empty[Variable, SimpleType]
    
    def transformConcrete(st: ConcreteType, pol: Boolean): ConcreteType = st match {
      case Record(fs) => Record(fs.map(f => f._1 -> transform(f._2, pol)))
      case Function(l, r) => Function(transform(l, !pol), transform(r, pol))
      case Primitive(_) | Top | Bot => st
    }
    def transform(st: SimpleType, pol: Boolean): SimpleType = st match {
      case v0: Variable =>
        val v = v0.representative
        mapping.getOrElseUpdate(v,
          if (v.lowerBound === v.upperBound) transformConcrete(v.lowerBound, pol)
          else if (pol && !neg(v)) transformConcrete(v.lowerBound, pol)
          else if (!pol && !pos(v)) transformConcrete(v.upperBound, pol)
          else new Variable(transformConcrete(v.lowerBound, true), transformConcrete(v.upperBound, false))
        )
      case c: ConcreteType => transformConcrete(c, pol)
    }
    transform(st, true)
    
  }
  
  
  type PolarVariable = (Variable, Boolean)
  
  /** Convert an inferred SimpleType into the immutable Type representation. */
  def coalesceType(st: SimpleType): Type = {
    val recursive = mutable.Map.empty[PolarVariable, TypeVariable]
    def go(st: SimpleType, polarity: Boolean)(inProcess: Set[PolarVariable]): Type = st match {
      case tv0: Variable =>
        val tv = tv0.representative
        val tv_pol = tv -> polarity
        if (inProcess.contains(tv_pol))
          recursive.getOrElseUpdate(tv_pol, freshVar.asTypeVar)
        else {
          val bound = if (polarity) tv.lowerBound else tv.upperBound
          val boundType = go(bound, polarity)(inProcess + tv_pol)
          val mrg = if (polarity) Union else Inter
          val res =
            if (polarity && bound === Bot || bound === Top) tv.asTypeVar
            else mrg(tv.asTypeVar, boundType)
          recursive.get(tv_pol).fold(res)(RecursiveType(_, res))
        }
      case Function(l, r) => FunctionType(go(l, !polarity)(inProcess), go(r, polarity)(inProcess))
      case Record(fs) => RecordType(fs.map(nt => nt._1 -> go(nt._2, polarity)(inProcess)))
      case Primitive(n) => PrimitiveType(n)
      case Top => PrimitiveType("⊤")
      case Bot => PrimitiveType("⊥")
    }
    go(st, true)(Set.empty)
  }
  
  
}
