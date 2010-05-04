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

    it("should parse loop statements") {
      val in = """
          while (lb > 1 && af(lb-1) < af(lb))
              lb = lb - 1;
          end
          """
      val out = "While(Conj(Equal(Var(lb),IntNum(1)),Equal(MCall(Var(af),List(Sub(Var(lb),IntNum(1))))," +
              "MCall(Var(af),List(Var(lb))))),Asgn(Id(lb),Sub(Var(lb),IntNum(1))))"
      strEqual(in, out)
    }

    it("should parse complex for statements") {
      val in = """
          for f = max(floor(res/3), 1):length(af),
            if (res == 1 || threshold*M <= af(f) && (f < lb || f > ub) && not(mod(f, res) < 2 || res - mod(f, res) < 2))
                bool = 0;
                return;
            end
          end
          """
      val out = "ForStmt(Asgn(Id(f),Range(MCall(Var(max),List(MCall(Var(floor),List(Div(Var(res),IntNum(3)))), " +
              "IntNum(1))),MCall(Var(length),List(Var(af))))),IfStmt(List((Disj(Equal(Var(res),IntNum(1))," +
              "Conj(Conj(Equal(Mul(Var(threshold),Var(M)),MCall(Var(af),List(Var(f)))),Disj(Equal(Var(f)," +
              "Var(lb)),Equal(Var(f),Var(ub)))),MCall(Var(not),List(Disj(Equal(MCall(Var(mod),List(Var(f), Var(res)))," +
              "IntNum(2)),Equal(Sub(Var(res),MCall(Var(mod),List(Var(f), Var(res)))),IntNum(2)))))))," +
              "MSeq(List(Asgn(Id(bool),IntNum(0)), Return)))),None))"
      strEqual(in, out)
    }

    it("should parse function calls") {
      val in = """
          if (abs(note - note_prev) < 0.2)
            note = note_prev
          else
            note = round(note);
          end
          """
      val out = "IfStmt(List((Equal(MCall(Var(abs),List(Sub(Var(note),Var(note_prev)))),FloatNum(0.20000000298023224))," +
              "Asgn(Id(note),Var(note_prev)))),Some(Asgn(Id(note),MCall(Var(round),List(Var(note))))))"
      strEqual(in, out)
    }

    it("should parse range expressions") {
      val in = """
          wcof = cof(iw_min:iw_max  ,
                     l);         % interpolation coefficients
          """
      val out = "Asgn(Id(wcof),MCall(Var(cof),List(Range(Var(iw_min),Var(iw_max)), Var(l))))"
      strEqual(in, out)
    }

    it("should parse multiplication and division") {
      val in = """
          2 * 3 / 4 *
            5;
          """
      val out = "Mul(Div(Mul(IntNum(2),IntNum(3)),IntNum(4)),IntNum(5))"
      strEqual(in, out)
    }

    it("should parse addition and subtraction") {
      val in = """
          2 * 3 - 4 +
            5 / 1;
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
            b1; b2; b3
          elseif ccc
            c1; c2;
          elseif ddd
            c3; c4;
          else d1; d2;
          end
          """
      val out = "IfStmt(List((Var(a),MSeq(List(Var(b1), Var(b2), Var(b3)))), (Var(ccc),MSeq(List(Var(c1), Var(c2)))), " +
              "(Var(ddd),MSeq(List(Var(c3), Var(c4))))),Some(MSeq(List(Var(d1), Var(d2)))))"
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