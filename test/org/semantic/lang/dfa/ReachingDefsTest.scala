package org.semantic.lang.dfa

import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

class ReachingDefsTest extends BasicCFATest {
  import DataFlow._

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

  test("reach (s3)") {
    expect(Set(s1, s2, s3))(reach(s3))
  }

  test("reach (s5)") {
    expect(Set(s1, s2, s3, s411, s412))(reach(s5))
  }

  test("reach s411") {
    expect(Set(s1, s2, s3, s411, s412))(reach(s411))  
  }


}