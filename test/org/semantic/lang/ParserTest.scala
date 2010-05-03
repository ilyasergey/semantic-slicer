package org.semantic.lang

import syntax.parsing.MatlabParser
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

/**
 * @author ilyas
 */

class ParserTest extends MatlabParser with Spec with ShouldMatchers {

  describe("MATLAB parser") {

    it("should parse multiplication and division") {
      val in = """
      2 * 3 / 4 * 5
      """
      val out = "MSeq(List(Mul(Div(Mul(IntNum(2),IntNum(3)),IntNum(4)),IntNum(5))))"
      parse(in).get.toString should equal(out)
    }

    it("should parse addition and subtraction") {
      val in = """
      2 * 3 - 4 + 5 / 1
      """
      val out = "MSeq(List(Add(Sub(Mul(IntNum(2),IntNum(3)),IntNum(4)),Div(IntNum(5),IntNum(1)))))"
      parse(in).get.toString should equal(out)
    }

    it("should parse integer, float and string literals") {
      val in = """
      42;
      239.05; ;
      'abc'
      """
      val out = "MSeq(List(IntNum(42), FloatNum(239.0500030517578), MString(abc)))"
      parse(in).get.toString should equal(out)
    }

    it("should parse conditional expressions") {

      val in = """
      if a,
        b1; b2; b3
      elseif 
        c1; c2
      elseif
        c3; c4
      else d1; d2
      end
      """
      val out = "MSeq(List(IfStmt(Var(a),MSeq(List(Var(b1), Var(b2), Var(b3))),List(MSeq(List(Var(c1), Var(c2))), MSeq(List(Var(c3), Var(c4))), MSeq(List(Var(d1), Var(d2)))))))"
      val result = parse(in).get
      result.toString should equal(out)

    }

  }

}