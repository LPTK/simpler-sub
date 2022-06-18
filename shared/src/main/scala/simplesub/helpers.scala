package simplesub

import scala.util.chaining._
import scala.collection.mutable.{Map => MutMap, SortedMap => SortedMutMap, Set => MutSet}
import scala.collection.immutable.SortedSet


// Helper methods for types

abstract class TypeImpl { self: Type =>
  
  lazy val typeVarsList: List[TypeVariable] = this match {
    case uv: TypeVariable => uv :: Nil
    case _ => children.flatMap(_.typeVarsList)
  }
  
  def show: String = {
    val vars = typeVarsList.distinct
    val ctx = vars.zipWithIndex.map {
      case (tv, idx) =>
        def nme = {
          assert(idx <= 'z' - 'a', "TODO handle case of not enough chars")
          ('a' + idx).toChar.toString
        }
        tv -> ("'" + nme)
    }.toMap
    showIn(ctx, 0)
  }
  
  private def parensIf(str: String, cnd: Boolean): String = if (cnd) "(" + str + ")" else str
  def showIn(ctx: Map[TypeVariable, String], outerPrec: Int): String = this match {
    case Top => "⊤"
    case Bot => "⊥"
    case PrimitiveType(name) => name
    case uv: TypeVariable => ctx(uv)
    case FunctionType(l, r) => parensIf(l.showIn(ctx, 11) + " -> " + r.showIn(ctx, 10), outerPrec > 10)
    case RecordType(fs) => fs.map(nt => s"${nt._1}: ${nt._2.showIn(ctx, 0)}").mkString("{", ", ", "}")
    case Union(l, r) => parensIf(l.showIn(ctx, 20) + " ∨ " + r.showIn(ctx, 20), outerPrec > 20)
    case Inter(l, r) => parensIf(l.showIn(ctx, 25) + " ∧ " + r.showIn(ctx, 25), outerPrec > 25)
  }
  
  def children: List[Type] = this match {
    case _: PrimitiveType | _: TypeVariable | Top | Bot => Nil
    case FunctionType(l, r) => l :: r :: Nil
    case RecordType(fs) => fs.map(_._2)
    case Union(l, r) => l :: r :: Nil
    case Inter(l, r) => l :: r :: Nil
  }
  
}
