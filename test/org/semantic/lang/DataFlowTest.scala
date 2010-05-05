package org.semantic.lang

import org.scalatest.FunSuite
import syntax.parsing.MatlabParser

/**
 * @author ilyas
 */

class DataFlowTest extends FunSuite {
  import org.semantic.lang.syntax._
  import org.semantic.lang.dfa.DataFlow._

  /*
  * {                     (prog)
  *     y = v             (s1)
  *     z = y             (s2)
  *     x = v             (s3)
  *     while x           (s4, s41)
  *         x = w         (s411)
  *         x = v         (s412)
  *     end
  *     return x          (s5)
  * }
  */

  val input = """
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
  val tree = parser.parse(input).get

  val s1 = Asgn(Var("y"), Var("v"))
  val s2 = Asgn(Var("z"), Var("y"))
  val s3 = Asgn(Var("x"), Var("v"))
  val s411 = Asgn(Var("x"), Var("w"))
  val s412 = Asgn(Var("x"), Var("v"))
  val s41 = MSeq(List(s411, s412))
  val s4 = While(Var("x"), s41)
  val s5 = MCall(Var("foo"), List(Var("x")))
  val prog = MSeq(List(s1, s2, s3, s4, s5))

  test("Parsing correctness") {
    // check tree
    expect(tree)(prog)
  }

  test("Simple CFA") {
    // Check successors
    expect(Set(s3))(succ(s2))
    expect(Set(s2))(succ(s1))
    expect(Set(s4))(succ(s412))
    expect(Set(s5, s41))(succ(s4))
  }

  test("in (s1)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(in(s1))
  }

  test("out (s4)") {
    expect(Set(Var("x"), Var("w"), Var("v"), Var("foo")))(out(s4))
  }


}