package org.semantic.lang.dfa

import org.scalatest.FunSuite
import org.semantic.lang.syntax._
import parsing.MatlabParser

/**
 * @author ilyas
 */

trait BasicCFATest extends FunSuite {
    /*
  * {                     (prog)
  *     y = v             (s1)
  *     z = y             (s2)
  *     x = v             (s3)
  *     while x           (s4, s41)
  *         x = w         (s411)
  *         x = v         (s412)
  *     end
  *     foo(x)            (s5)
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

}