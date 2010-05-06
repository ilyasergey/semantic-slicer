package org.semantic.lang.dfa

import org.scalatest.FunSuite
import org.scalatest.FunSuite
import org.semantic.lang.syntax._
import parsing.MatlabParser

/**
 * @author ilyas
 */

class InputOutputTest extends FunSuite with InputOutputImpl{
  private def findSelected(stmt: MStmt) = {
    def find(s: MStmt): Selected = {
      s match {
        case sel: Selected => return sel
        case _ => for (c <- s.children; if c.isInstanceOf[MStmt]) {
          val sel = find(c.asInstanceOf[MStmt])
          if (sel != null) return sel
        }
      }
      return null
    }

    find(stmt)
  }

  val input = """
       y = v
       z = y
       #begin
       x = v
       while x
           x = w
           x = v
       end
       #end
       foo(x)
    """
  val parser = new MatlabParser
  val tree = parser.parse(input).get
  val selection = findSelected(tree)

  test("Simple input/output") {
    expect(Set(Var("v"), Var("x"), Var("w")))(inputVariables(selection))
  }

}