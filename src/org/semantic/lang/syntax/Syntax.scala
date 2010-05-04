package org.semantic.lang.syntax

import kiama.attribution.Attributable

/**
 * @author ilyas
 *
 * MATLAB syntax
 */

trait ASTNode

/**
 * MATLAB identifier
 */
case class Id(name: String) extends ASTNode

/**
 * Statements
 */
trait MStmt extends ASTNode with Attributable

/**
 * Label trait for loops
 */
trait LoopStmt extends MStmt

case class MSeq(ss: Seq[MStmt]) extends MStmt
case class IfStmt(thenBranches: Seq[(MExp, MStmt)], elseBranch: Option[MStmt]) extends MStmt
case class While(c: MExp, b: MStmt) extends MStmt with LoopStmt
case class ForStmt(a: Asgn, body: MStmt) extends MStmt with LoopStmt
case class Asgn(name: Var, e: MExp) extends MStmt
case class ListAsgn(names: Seq[Var], e: MExp) extends MStmt
case object Return extends MStmt
case object Break extends MStmt
case object Continue extends MStmt


abstract class MExp extends MStmt
abstract case class BinaryExp(_l: MExp, _r: MExp) extends MExp

case class IntNum(i: Int) extends MExp
case class FloatNum(f: Double) extends MExp
case class MString(s: String) extends MExp
case class Var(s: String) extends MExp

// Unary
case class Neg(e: MExp) extends MExp
case class Transp(e: MExp) extends MExp

case class Equal(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Add(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Sub(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Mul(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Div(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Pow(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Range(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Conj(l: MExp, r: MExp) extends BinaryExp(l, r)
case class Disj(l: MExp, r: MExp) extends BinaryExp(l, r)

/**
 * Function call
 */
case class MCall(name: Var, args: Seq[MExp]) extends MExp


/**
 * Function definition
 */
case class MFunction(name: Id, res: Seq[Id], params: Seq[Id])