package org.semantic.lang.dfa

import org.semantic.lang.dfa.DataFlow._

/**
 * @author ilyas
 */

class ControlFlowTest extends BasicCFATest {

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
}