package org.semantic.lang.syntax

/**
 * @author ilyas
 *
 * Standard MATLAB expression
 */

abstract class MExp extends MStmt

case class IntNum (i : Int) extends MExp
case class FloatNum (f : Double) extends MExp
case class MString (s : String) extends MExp

case class Var (s : String) extends MExp

case class Neg (e : MExp) extends MExp
case class Add (l : MExp, r : MExp) extends MExp
case class Sub (l : MExp, r : MExp) extends MExp
case class Mul (l : MExp, r : MExp) extends MExp
case class Div (l : MExp, r : MExp) extends MExp

/**
 * Function call
 */
case class FCall(name: Id, args: Seq[MExp]) extends MExp
