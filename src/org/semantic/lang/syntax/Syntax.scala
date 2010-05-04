package org.semantic.lang.syntax

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
abstract class MStmt extends ASTNode

case class MSeq(ss: Seq[MStmt]) extends MStmt
case class IfStmt(thenBranches: Seq[(MExp, MStmt)], elseBranch: Option[MStmt]) extends MStmt
case class While(c: MExp, b: MStmt) extends MStmt
case class Asgn(name: Id, e: MExp) extends MStmt
case class ListAsgn(names: Seq[Id], e: MExp) extends MStmt
case class ForStmt(a: Asgn, body: MStmt) extends MStmt
case object Return extends MStmt
case object Break extends MStmt
case object Continue extends MStmt


abstract class MExp extends MStmt

case class IntNum(i: Int) extends MExp
case class FloatNum(f: Double) extends MExp
case class MString(s: String) extends MExp
case class Var(s: String) extends MExp
case class Neg(e: MExp) extends MExp
case class Equal(l: MExp, r: MExp) extends MExp
case class Add(l: MExp, r: MExp) extends MExp
case class Sub(l: MExp, r: MExp) extends MExp
case class Mul(l: MExp, r: MExp) extends MExp
case class Div(l: MExp, r: MExp) extends MExp
case class Pow(l: MExp, r: MExp) extends MExp
case class Range(l: MExp, r: MExp) extends MExp
case class Transp(e: MExp) extends MExp
case class Conj(l: MExp, r: MExp) extends MExp
case class Disj(l: MExp, r: MExp) extends MExp

/**
 * Function call
 */
case class MCall(name: Id, args: Seq[MExp]) extends MExp


/**
 * Function definition
 */
case class MFunction(name: Id, res: Seq[Id], params: Seq[Id])