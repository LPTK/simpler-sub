package simplesub

import org.scalatest._
import fastparse._
import Parser.expr
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import org.scalatest.funsuite.AnyFunSuite

class ParserTests extends AnyFunSuite {
  
  def doTest(str: String): Unit = {
    parse(str, expr(_), verboseFailures = true) match {
      case Success(value, index) =>
        // println("OK: " + value)
      case f: Failure =>
        val Failure(expected, failIndex, extra) = f
        println(extra.trace())
        println(extra.trace().longAggregateMsg)
        assert(false)
    }
    ()
  }
  
  test("basics") {
    doTest("1")
    doTest("a")
    doTest("1 2 3")
    doTest("a b c")
    doTest("true")
  }
  
  test("let") {
    doTest("let a = b in c")
    doTest("let a = 1 in 1")
    doTest("let a = (1) in 1")
    assert(!parse("let true = 0 in true", expr(_)).isSuccess)
  }
  
  test("records") {
    doTest("{ a = 1; b = 2 }")
    assert(!parse("{ a = 1; b = 2; a = 3 }", expr(_)).isSuccess)
  }
  
}
