package org.semantic.lang.dfa

import kiama.attribution.DynamicAttribution._
import org.semantic.lang.syntax._
import kiama.attribution.Attributable
import collection.mutable.Map


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
   * Transitive successors
   */
  val succTrans: MStmt ==> Set[MStmt]

  /**
   *  Control flow predcessor relation.
   */
  val pred: MStmt ==> Set[MStmt]


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
    case s => s -> following
  }

  val succTrans: MStmt ==> Set[MStmt] = attr {
    case s =>
      import scala.collection.mutable.HashSet
      val acc = new HashSet[MStmt]

      def collect(stmt: MStmt) {
        acc ++ succ(stmt)
        for (s1 <- succ(stmt) if !acc.contains(s1)) {
          collect(s1)
        }
      }
      collect(s)
      Set(acc.toSeq: _*)
  }

  val pred: MStmt ==> Set[MStmt] = attr {
    case s => predMap(getRoot(s)).get(s).getOrElse(Set())
  }

  private def getRoot(stmt: MStmt): MStmt = stmt.parent match {
    case s: MStmt => getRoot(s)
    case _ => stmt
  }

  private def predMap(stmt: MStmt): Map[MStmt, Set[MStmt]] = {
    import scala.collection.mutable.{HashMap => MuMap}

    val succMap = new MuMap[MStmt, Set[MStmt]]
    def collect(st: MStmt) {
      val localSuccessors = succ(st)
      succMap += ((st, localSuccessors))
      for (suc <- localSuccessors.toList.filter(_.isInstanceOf[MStmt])) {
        if (!succMap.keySet.contains(suc)) collect((suc))
      }
    }

    collect(stmt)

    /*
        println("----------------")
        for (e <- succMap) println (e)
        println("----------------")
    */

    import scala.collection.mutable.{HashMap => MuMap}
    val predSet = new MuMap[MStmt, Set[MStmt]]
    for ((currentStmt, sucSet) <- succMap; sucStmt <- sucSet) {
      val tmp = predSet.getOrElseUpdate(sucStmt, Set())
      predSet.update(sucStmt, tmp + currentStmt)
    }
    predSet
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
    case MSeq(s) => Set(s.flatMap(uses(_)): _*)
    case BinaryExp(l, r) => (l -> uses) ++ (r -> uses)
    case UnaryExp(e) => e -> uses
    case MCall(v, args) => (v -> uses) ++ (Set[Var]() /: args)((s, e) => s ++ (e -> uses))
    case Asgn(_, e) => e -> uses
    case IfStmt(tb, eb) => (Set[Var]() /: tb)((s, e) => s ++ uses(e._1) ++ uses(e._2)) ++
            eb.map(uses(_)).getOrElse(Set())
    case While(c, s) => (c -> uses) ++ (s -> uses)
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
   *
   * in (s) contains all variables that have values that are defined before s 
   * and are used either in s or in later statements without first being redefined
   */
  val in: MStmt ==> Set[Var]

  /**
   * Variables "live" out of a statement.
   *
   * out (s) contains all variables whose values either pass through s
   * or are defined by s and are used later without first being redefined
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
    // For all successors s compute in(s) --- this will be `local' out of current statement 
    case s => (s -> succ) flatMap (in)
  }

}


object DataFlow extends LivenessImpl with VariablesImpl with ControlFlowImpl with ReachingDefinitionsImpl
