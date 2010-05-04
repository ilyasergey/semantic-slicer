package org.semantic.lang.syntax

/**
 * @author ilyas
 *
 * Basic MATLAB statements
 */


/**
 * Sequence of statements
 */


/**
 * Statements
 */
abstract class MStmt

case class MSeq(ss: Seq[MStmt]) extends MStmt

case class IfStmt(c: MExp, thenBranch: MStmt, elseBranches: Seq[MStmt]) extends MStmt

case class While(c: MExp, b: MStmt) extends MStmt

case class Asgn(name: Id, e: MExp) extends MStmt

case class ListAsgn(names: Seq[Id], e: MExp)