package simplesub

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class IsolatedTests extends TypingTestHelpers {
  
  // This test class is for isolating single tests and running them alone
  // with sbt command `~testOnly simplesub.IsolatedTests`
  
  test("isolated") {
    
    // put your test here
    
    doTest("fun f -> fun x -> f (f x)")
    
    doTest("not true")
    doTest("(fun x -> not x) true")
    doTest("fun x -> not x")
    doTest("fun x -> { u = not x; v = x }")
    
    doTest("if true then { u = 1; v = 2; w = 3 } else { u = true; v = 4; x = 5 }")
    doTest("if true then fun x -> { u = 1; v = x } else fun y -> { u = y; v = y }")
    
    error("fun x -> x x", "Illegal cyclic constraint: α0 <: (α0 -> α1)")
    
  }
  
}
