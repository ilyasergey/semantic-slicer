package org.semantic.lang

import org.scalatest.FunSuite
import syntax.parsing.MatlabParser

/**
 * @author ilyas
 */

class ControlFlowTest extends FunSuite {
  import org.semantic.lang.syntax._
  import org.semantic.lang.controlFlow._

  test("test 1") {
    val in = """
       y = v
       z = y
       x = v
       while x
           x = w
           x = v
       end
       foo(x)
    """
    val parser = new MatlabParser
    val tree = parser.parse(in).get

    val s1 = Asgn(Id("y"), Var("v"))
    val s2 = Asgn(Id("z"), Var("y"))
    val s3 = Asgn(Id("x"), Var("v"))
    val s411 = Asgn(Id("x"), Var("w"))
    val s412 = Asgn(Id("x"), Var("v"))
    val s41 = MSeq(List(s411, s412))
    val s4 = While(Var("x"), s41)
    val s5 = MCall(Var("foo"), List(Var("x")))
    val prog = MSeq(List (s1, s2, s3, s4, s5))

    assert(tree == prog)

  }


}