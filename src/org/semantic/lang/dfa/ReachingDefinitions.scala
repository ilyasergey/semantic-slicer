package org.semantic.lang.dfa

import kiama.attribution.DynamicAttribution._
import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

trait Assignments {

  /**
   * Assignments
   */
  val assigns: MStmt ==> Set[Asgn]
}

trait AssignmentsImpl extends Assignments {
  val assigns: MStmt ==> Set[Asgn] =
  attr {
    case a@Asgn(_, _) => Set(a)
    case _ => Set()
  }

}

trait ReachingDefinitions {

  /**
   * Set of reaching assignments
   */
  val reach: MStmt ==> Set[Asgn]


}

trait ReachingDefinitionsImpl extends ReachingDefinitions
        with ControlFlowImpl with AssignmentsImpl {
  val reach: MStmt ==> Set[Asgn] =
  circular(Set[Asgn]()) {
    case s => assigns(s) ++ outAsgn(s)
  }

  private val outAsgn: MStmt ==> Set[Asgn] =
  circular(Set[Asgn]()) {
    case s => {
      s -> pred flatMap (reach)
    }
  }


}