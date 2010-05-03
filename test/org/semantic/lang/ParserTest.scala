package org.semantic.lang

import syntax.parsing.MatlabParser
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

/**
 * @author ilyas
 */

class ParserTest extends MatlabParser with Spec with ShouldMatchers {

  describe("MATLAB parser") {

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