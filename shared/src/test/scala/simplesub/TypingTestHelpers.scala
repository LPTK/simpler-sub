package simplesub

import org.scalatest._
import fastparse._
import Parser.expr
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import sourcecode.Line
import org.scalatest.funsuite.AnyFunSuite
import ammonite.ops._

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class TypingTestHelpers extends AnyFunSuite {
  
  private var outFile = Option.empty[os.Path]
  
  override protected def test(testName: String, testTags: Tag*)(testFun: => Any)(implicit pos: org.scalactic.source.Position): Unit = {
    super.test(testName, testTags: _*) {
      assert(outFile.isEmpty)
      val f = pwd/"out"/(testName+".check")
      outFile = Some(f)
      write.over(f, "")
      try testFun
      finally outFile = None
    }
  }
  
  def doTest(str: String, expected: String = "", expectError: Boolean = false)(implicit line: Line): Unit = {
    val dbg = expected.isEmpty
    
    if (dbg) println(s">>> $str")
    val Success(term, index) = parse(str, expr(_), verboseFailures = true)
    
    val typer = new Typer(dbg)
    val res = try {
      val tyv = typer.inferType(term)
      
      if (dbg) {
        println("inferred: " + tyv)
        println(" where " + tyv.showBounds)
      }
      
      val res0 = typer.simplifyType(tyv)
      if (dbg) {
        println("simplified: " + res0)
        println(" where " + res0.showBounds)
      }
      
      val res = typer.coalesceType(res0).show
      
      if (dbg) {
        println("typed: " + res)
        println("---")
      } else {
        // assert(res == expected, "at line " + line.value); ()
      }
      
      res
    } catch {
      case e: TypeError =>
        if (dbg) {
          println("ERROR: " + e.msg)
          println("---")
        }
        "// ERROR: " + e.msg
    }
    write.append(outFile.getOrElse(fail()),
      "// " + (if (expectError) "[wrong:] " else "") + str + "\n" + res + "\n\n", createFolders = true)
  }
  def error(str: String, msg: String): Unit = {
    // assert(intercept[TypeError](doTest(str, "<none>")).msg == msg); ()
    doTest(str, "<none>", true)
  }
  
}
