package org.semantic.lang.dfa

import kiama.attribution.DynamicAttribution._
import org.semantic.lang.syntax._
import kiama.attribution.Attributable


/**
 * @author ilyas
 */

/**
 * Control flow interface.
 */
trait ControlFlow {

  /**
   * Control flow successor relation.
   */
  val succ: MStmt ==> Set[MStmt]


  /**
   * Control flow default successor relation.
   * see pending edges in IDEA DFA implementation
   * following depends on the parent context so we use a childAttr to define it
   */
  val following: MStmt ==> Set[MStmt]
}


/**
 * Control flow implementation.
 */
trait ControlFlowImpl extends ControlFlow {
  val succ: MStmt ==> Set[MStmt] = attr {
    case IfStmt(tbs, eb) => {
      val cts = tbs.flatMap {case (x, y) => Seq(x, y)}
      val elseBranch = eb match {
        case Some(x) => Set(x)
        case None => Set()
      }
      Set(cts: _*) ++ elseBranch
    }
    case t@While(_, s) => t -> following + s
    case MSeq(s :: _) => Set(s)
    case Return => Set()

    // todo not sure that this is necessary...
    //    case BinaryExp(l, _) => Set(l)
    //    case MCall(v, s :: _) => Set(v)
    case s => s -> following
  }

  val following: MStmt ==> Set[MStmt] = childAttr {
    case Break => {
      case s => parentOfType(s, classOf[LoopStmt]).map(_ -> following).getOrElse(Set())
    }
    case Continue => {
      case s => parentOfType(s, classOf[LoopStmt]).map(p => Set(p.asInstanceOf[MStmt])).getOrElse(Set())
    }
    case currentNode => { // match by parent
      case t@IfStmt(_, _) => t -> following
      case t@While(_, _) => Set(t)
      case seq@MSeq(_) if currentNode isLast => seq -> following
      case MSeq(_) => Set(currentNode.next)

      // todo not sure that this is necessary...
      //      case MCall(v, s :: _) if currentNode eq v => Set(s)
      //      case c@MCall(_, _) if currentNode isLast => c -> following
      //      case MCall(_, _) => Set(currentNode.next)
      //      case b@BinaryExp(_, r) if currentNode eq r => b -> following
      //      case BinaryExp(l, r) if currentNode eq l => Set(r)

      case _ => Set()
    }
  }

  private def parentOfType[T <: Attributable](a: Attributable, c: Class[T]): Option[T] = a match {
    case null => None
    case x: T if x.getClass == c => Some(x)
    case _ => parentOfType(a.parent, c)
  }

}

/**
 * Variable use and definition interface.
 */
trait Variables {

  /**
   * Variable uses.
   */
  val uses: MStmt ==> Set[Var]

  /**
   * Variable definitions.
   */
  val defines: MStmt ==> Set[Var]
}

trait VariablesImpl extends Variables {
  val uses: MStmt ==> Set[Var] =
  attr {
    case v@Var(_) => Set(v)
    case Asgn(_, e) => e -> uses
    case IfStmt(tb, _) => (Set[Var]() /: tb.map(_._1))((s, e) => s ++ uses(e))
    case While(c, _) => c -> uses
    case _ => Set()
  }

  val defines: MStmt ==> Set[Var] =
  attr {
    case Asgn(v, _) => Set(v)
    case _ => Set()
  }

}

/**
 * Variable liveness interface.
 */
trait Liveness {

  /**
   * Variables "live" into a statement.
   */
  val in: MStmt ==> Set[Var]

  /**
   * Variables "live" out of a statement.
   */
  val out: MStmt ==> Set[Var]

}

/**
 * Variable liveness implementation.
 */
trait LivenessImpl extends Liveness {
  self: Liveness with Variables with ControlFlow =>

  val in: MStmt ==> Set[Var] =
  circular(Set[Var]()) {
    case s => uses(s) ++ (out(s) -- defines(s))
  }

  val out: MStmt ==> Set[Var] =
  circular(Set[Var]()) {
    case s => (s -> succ) flatMap (in)
  }

}


object DataFlow extends LivenessImpl with VariablesImpl with ControlFlowImpl
