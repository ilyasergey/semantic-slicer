package org.semantic.lang

import syntax.parsing.MatlabParser
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

/**
 * @author ilyas
 */

class ParserTest extends MatlabParser with Spec with ShouldMatchers {
  describe("MATLAB parser") {

    /*
        it("should parse transposition") {
          val in = """
          a'
          """
          val out = ""
          parse(in).get.toString should equal(out)
        }
    */

    it("should parse function calls") {
      val in = """
      if (abs(note - note_prev) < 0.2)
        note = note_prev
      else
        note = round(note);
      end
      """
      val out = "IfStmt(Equal(MCall(Id(abs),List(Sub(Var(note),Var(note_prev)))),FloatNum(0.20000000298023224))," +
              "Asgn(Id(note),Var(note_prev)),List(Asgn(Id(note),MCall(Id(round),List(Var(note))))))"
      strEqual(in, out)
    }

    it("should parse range expressions") {
      val in = """
      wcof = cof(iw_min:iw_max,l);         % interpolation coefficients
      """
      val out = "Asgn(Id(wcof),MCall(Id(cof),List(Range(Var(iw_min),Var(iw_max)), Var(l))))"
      strEqual(in, out)
    }

    it("should parse multiplication and division") {
      val in = """
      2 * 3 / 4 * 5;
      """
      val out = "Mul(Div(Mul(IntNum(2),IntNum(3)),IntNum(4)),IntNum(5))"
      strEqual(in, out)
    }

    it("should parse addition and subtraction") {
      val in = """
      2 * 3 - 4 + 5 / 1;
      """
      val out = "Add(Sub(Mul(IntNum(2),IntNum(3)),IntNum(4)),Div(IntNum(5),IntNum(1)))"
      strEqual(in, out)
    }

    it("should parse power exprs") {
      val in = """
      2 ^ (3 - 1);
      """
      val out = "Pow(IntNum(2),Sub(IntNum(3),IntNum(1)))"
      strEqual(in, out)
    }

    it("should parse integer, float and string literals") {
      val in = """
      42;
      239.05; ;
      'abc';
      """
      val out = "MSeq(List(IntNum(42), FloatNum(239.0500030517578), MString(abc)))"
      strEqual(in, out)
    }

    it("should parse conditional expressions") {

      val in = """
      if a,
        b1; b2; b3;
      elseif
        c1; c2;
      elseif
        c3; c4;
      else d1; d2;
      end
      """
      val out = "IfStmt(Var(a),MSeq(List(Var(b1), Var(b2), Var(b3))),List(MSeq(List(Var(c1), Var(c2))), MSeq(List(Var(c3), Var(c4))), MSeq(List(Var(d1), Var(d2)))))"
      strEqual(in, out)
    }
  }

  def strEqual(in: String, out: String): Any = {
    val result = parse(in)
    result match {
      case n: NoSuccess => {
        println(n)
        assert(false)
      }
      case s => s.get.toString should equal(out)
    }
  }


}