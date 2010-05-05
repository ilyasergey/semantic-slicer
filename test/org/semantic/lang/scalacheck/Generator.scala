package org.semantic.lang.scalacheck

import org.scalacheck.Properties
import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

trait Generator {
  import org.scalacheck._
  import org.semantic.lang.syntax._

  val genInteger = for (i <- Gen.choose(1, 100)) yield IntNum(i)
  val genDouble = for (i <- Gen.choose(1.0, 1000000.0)) yield FloatNum(i)
  val genNum = Gen.frequency((3, genInteger), (1, genDouble))

  implicit def arbNum: Arbitrary[MExp] =
    Arbitrary(genNum)

  val genIdn: Gen[String] = for (s <- Gen.identifier) yield (s.take(5))
  val genVar = for (v <- genIdn) yield Var(v)

  val genLeafExp = Gen.oneOf(genNum, genVar)

  def genNeg(sz: Int) =
    for{e <- genExp(sz / 2)} yield Neg(e)

  def genAdd(sz: Int) =
    for{l <- genExp(sz / 2); r <- genExp(sz / 2)} yield Add(l, r)

  def genSub(sz: Int) =
    for{l <- genExp(sz / 2); r <- genExp(sz / 2)} yield Sub(l, r)

  def genMul(sz: Int) =
    for{l <- genExp(sz / 2); r <- genExp(sz / 2)} yield Mul(l, r)

  def genDiv(sz: Int) =
    for{l <- genExp(sz / 2); r <- genExp(sz / 2)} yield Div(l, r)

  // todo generate sequence of arguments
  def genCall(sz: Int) =
    for{id <- genVar; e <- genExp(sz)} yield MCall(id, List(e))

  def genInternalExp(sz: Int) =
    Gen.oneOf(genAdd(sz), genSub(sz), genMul(sz), genDiv(sz), genCall(sz))

  def genExp(sz: Int): Gen[MExp] =
    if (sz <= 0)
      genLeafExp
    else
      Gen.frequency((1, genLeafExp), (3, genInternalExp(sz)))

  implicit def arbExp: Arbitrary[MExp] =
    Arbitrary {Gen.sized(sz => genExp(sz))}

  val genLeafStmt = genVar

  def genSeqn(sz: Int) =
    for{len <- Gen.choose(1, sz)
        ss <- Gen.containerOfN[List, MStmt](len, genStmt(sz / len))}
    yield MSeq(ss)

  implicit def arbSeqn: Arbitrary[MSeq] =
    Arbitrary {Gen.sized(sz => genSeqn(sz))}

  def genAsgn(sz: Int) =
    for{i <- genVar; e <- genExp(sz - 1)} yield Asgn(i, e)

  implicit def arbAsgn: Arbitrary[Asgn] =
    Arbitrary {Gen.sized(sz => genAsgn(sz))}

  def genWhile(sz: Int) =
    for{e <- genExp(sz / 3); b <- genStmt(sz - 1)} yield While(e, b)

  implicit def arbWhile: Arbitrary[While] =
    Arbitrary {Gen.sized(sz => genWhile(sz))}

  def genFor(sz: Int) =
    for{a <- genAsgn(sz / 3); stm <- genStmt(sz - 1)} yield ForStmt(a, stm)

  implicit def arbFor: Arbitrary[ForStmt] =
    Arbitrary {Gen.sized(sz => genFor(sz))}

  def genInternalStmt(sz: Int): Gen[MStmt] =
    Gen.frequency((1, genSeqn(sz)), (5, genAsgn(sz)), (3, genWhile(sz)), (2, genFor(sz)))

  def genStmt(sz: Int) =
    if (sz <= 0)
      genLeafStmt
    else
      Gen.frequency((1, genLeafStmt), (9, genInternalStmt(sz)))

  implicit def arbStmt: Arbitrary[MStmt] =
    Arbitrary {Gen.sized(sz => genStmt(sz))}
}

object MatlabProgramProperties extends Properties("MATLAB program") with Generator {
  import org.scalacheck.Prop.forAll

  property("is syntacticall correct") = forAll{(p: MStmt) =>
    p.isInstanceOf[ASTNode]
  }
}