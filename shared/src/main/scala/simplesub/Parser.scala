package simplesub

import scala.util.chaining._
import fastparse._, fastparse.ScalaWhitespace._

@SuppressWarnings(Array("org.wartremover.warts.All"))
object Parser {
  
  val keywords = Set("let", "rec", "in", "fun", "if", "then", "else", "true", "false")
  def kw[__ : P](s: String) = s ~~ !(letter | digit | "_" | "'")
  
  def letter[__ : P]     = P( lowercase | uppercase )
  def lowercase[__ : P]  = P( CharIn("a-z") )
  def uppercase[__ : P]  = P( CharIn("A-Z") )
  def digit[__ : P]      = P( CharIn("0-9") )
  def number[__ : P]: P[Int] = P( CharIn("0-9").repX(1).!.map(_.toInt) )
  def ident[__ : P]: P[String] =
    P( (letter | "_") ~~ (letter | digit | "_" | "'").repX ).!.filter(!keywords(_))
  
  def term[__ : P]: P[Term] = P( let | fun | ite | apps )
  def const[__ : P]: P[Term] = number.map(Lit)
  def variable[__ : P]: P[Term] = (ident | "true".! | "false".!).map(Var)
  def parens[__ : P]: P[Term] = P( "(" ~/ term ~ ")" )
  def subtermNoSel[__ : P]: P[Term] = P( parens | record | const | variable )
  def subterm[__ : P]: P[Term] = P( subtermNoSel ~ ("." ~/ ident).rep ).map {
    case (st, sels) => sels.foldLeft(st)(Sel) }
  def record[__ : P]: P[Term] = P( "{" ~/ (ident ~ "=" ~ term).rep(sep = ";") ~ "}" )
    .filter(xs => xs.map(_._1).toSet.size === xs.size).map(_.toList pipe Rcd)
  def fun[__ : P]: P[Term] = P( kw("fun") ~/ ident ~ "->" ~ term ).map(Lam.tupled)
  def let[__ : P]: P[Term] =
    P( kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ ident ~ "=" ~ term ~ kw("in") ~ term )
    .map(Let.tupled)
  def ite[__ : P]: P[Term] = P( kw("if") ~/ term ~ kw("then") ~ term ~ kw("else") ~ term ).map(ite =>
    App(App(App(Var("if"), ite._1), ite._2), ite._3))
  def apps[__ : P]: P[Term] = P( subterm.rep(1).map(_.reduce(App)) )
  
  def expr[__ : P]: P[Term] = P( term ~ End )
  
  def toplvl[__ : P]: P[(Boolean, String, Term)] =
    P( kw("let") ~/ kw("rec").!.?.map(_.isDefined) ~ ident ~ "=" ~ term )
  def pgrm[__ : P]: P[Pgrm] = P( ("" ~ toplvl).rep.map(_.toList) ~ End ).map(Pgrm)
  
}
