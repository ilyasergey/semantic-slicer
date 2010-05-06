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
sealed trait MStmt extends ASTNode with Attributable

/**
 * Label trait for loops
 */
sealed trait LoopStmt extends MStmt

case class MSeq(ss: Seq[MStmt]) extends MStmt
case class IfStmt(thenBranches: Seq[(MExp, MStmt)], elseBranch: Option[MStmt]) extends MStmt
case class While(c: MExp, b: MStmt) extends MStmt with LoopStmt
case class ForStmt(a: Asgn, body: MStmt) extends MStmt with LoopStmt
case class Asgn(name: Var, e: MExp) extends MStmt {
  val tag: Int = Counter.asgnTag

  override def equals(obj: Any) = _equals(obj) && (obj match {
    case v: Asgn => this.tag == v.tag
    case _ => false
  })


  override def hashCode = {
    _hashCode * 31 + tag
  }


  private def _hashCode = super.hashCode

  private def _equals(obj: Any) = super.equals(obj)
  /*
    def raw = new Asgn(name, e) {
      override def hashCode = _hashCode
      override def equals(obj: Any) = _equals(obj)
    }
  */

}
case class ListAsgn(names: Seq[Var], e: MExp) extends MStmt
case object Return extends MStmt
case object Break extends MStmt
case object Continue extends MStmt


// todo implement subexpressions!
sealed abstract class MExp extends MStmt
sealed abstract case class BinaryExp(_l: MExp, _r: MExp) extends MExp
sealed abstract case class UnaryExp(_e: MExp) extends MExp

case class IntNum(i: Int) extends MExp
case class FloatNum(f: Double) extends MExp
case class MString(s: String) extends MExp


case class Var(s: String) extends MExp {
  val tag: Int = Counter.varTag

  /*
    override def equals(obj: Any) = super.equals(obj) && (obj match {
      case v:Var => this.tag == v.tag
      case _ => false
    })

    override def hashCode = super.hashCode * 31 + tag
  */
}

// Unary
case class Neg(e: MExp) extends UnaryExp(e)
case class Transp(e: MExp) extends UnaryExp(e)

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

private[syntax] object Counter {
  private var varCounter = 0;
  private var asgnCounter = 0;

  def varTag: Int = {
    val tmp = varCounter
    varCounter += 1
    tmp
  }

  def asgnTag: Int = {
    val tmp = asgnCounter
    asgnCounter += 1
    tmp
  }
}