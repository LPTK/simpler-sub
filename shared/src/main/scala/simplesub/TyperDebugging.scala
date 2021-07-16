package simplesub

import scala.collection.mutable.{Map => MutMap, Set => MutSet, LinkedHashMap, LinkedHashSet}
import scala.collection.immutable.{SortedMap, SortedSet}
import scala.annotation.tailrec

/** Inessential methods used to help debugging. */
abstract class TyperDebugging { self: Typer =>
  
  // Shadow Predef functions with debugging-flag-enabled ones:
  def println(msg: => Any): Unit = if (dbg) emitDbg(" " * indent + msg)
  def assert(assertion: => Boolean): Unit = if (dbg) scala.Predef.assert(assertion)
  
  private val noPostTrace: Any => String = _ => ""
  
  protected var indent = 0
  def trace[T](pre: String)(thunk: => T)(post: T => String = noPostTrace): T = {
    println(pre)
    indent += 1
    val res = try thunk finally indent -= 1
    if (post isnt noPostTrace) println(post(res))
    res
  }
  def emitDbg(str: String): Unit = scala.Predef.println(str)
  
  trait SimpleTypeImpl { self: SimpleType =>
    
    def children: List[SimpleType] = this match {
      case tv: Variable => tv.lowerBound :: tv.upperBound :: Nil
      case Function(l, r) => l :: r :: Nil
      case Record(fs) => fs.map(_._2)
      case Primitive(_) => Nil
      case Top | Bot => Nil
    }
    def getVars: Set[Variable] = {
      val res = MutSet.empty[Variable]
      @tailrec def rec(queue: List[SimpleType]): Unit = queue match {
        case (tv0: Variable) :: tys =>
          val tv = tv0.representative
          if (res(tv)) rec(tys)
          else { res += tv; rec(tv.children ::: tys) }
        case ty :: tys => rec(ty.children ::: tys)
        case Nil => ()
      }
      rec(this :: Nil)
      SortedSet.from(res)(Ordering.by(_.uid))
    }
    def show: String = coalesceType(this).show
    def showBounds: String =
      getVars.iterator.filter(tv => tv.upperBound =/= Top || tv.lowerBound =/= Bot).map(tv =>
        tv.toString
          + (if (tv.lowerBound === Bot) "" else " :> " + tv.lowerBound)
          + (if (tv.upperBound === Top) "" else " <: " + tv.upperBound)
      ).mkString(", ")
    
  }
  
}
