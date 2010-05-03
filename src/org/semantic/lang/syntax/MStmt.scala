package org.semantic.lang.syntax

/**
 * @author ilyas
 *
 * Basic MATLAB statements
 */


/**
 * Sequence of statements
 */
case class MSeq(ss: Seq[MStmt])


/**
 * Statements
 */
abstract class MStmt

case class IfStmt(c: MExp, thenBranch: MSeq, elseBranches: Seq[MSeq]) extends MStmt

case class While(c: MExp, b: MStmt) extends MStmt

case class Asgn(name: Id, e: MExp) extends MStmt

case class ListAsgn(names: Seq[Id], e: MExp)