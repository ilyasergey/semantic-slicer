package org.semantic.lang.dfa

import org.semantic.lang.syntax._
import org.semantic.lang.dfa.DataFlow._

/**
 * @author ilyas
 */

class LivenessTest extends BasicCFATest {

  test("in (s1)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(in(s1))
  }

  test("in (s2)") {
    expect(Set(Var("y"), Var("w"), Var("v"), Var("foo")))(in(s2))
  }

  test("in (s3)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(in(s3))
  }

  test("in (s4)") {
    expect(Set(Var("x"), Var("w"), Var("v"), Var("foo")))(in(s4))
  }

  test("in (s411)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(in(s411))
  }

  test("in (s412)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(in(s412))
  }

  test("in (s5)") {
    expect(Set(Var("x"), Var("foo")))(in(s5))
  }

  test("out (s1)") {
    expect(Set(Var("y"), Var("w"), Var("foo"), Var("v")))(out(s1))
  }

  test("out (s2)") {
    expect(Set(Var("w"), Var("foo"), Var("v")))(out(s2))
  }

  test("out (s3)") {
    expect(Set(Var("x"), Var("foo"), Var("w"), Var("v")))(out(s3))
  }

  test("out (s4)") {
    expect(Set(Var("x"), Var("w"), Var("v"), Var("foo")))(out(s4))
  }
  test("out (s411)") {
    expect(Set(Var("w"), Var("v"), Var("foo")))(out(s411))
  }

  test("out (s412)") {
    expect(Set(Var("x"), Var("w"), Var("v"), Var("foo")))(out(s412))

  }

  test("out (s5)") {
    expect(Set())(out(s5))
  }

}