package org.semantic.lang.dfa

import kiama.attribution.DynamicAttribution._
import org.semantic.lang.syntax._
import kiama.attribution.Attributable


/**
 * @author ilyas
 */

trait Assignments {

  /**
   * Assignments
   */
  val defines: MStmt ==> Set[Asgn]
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

  val reach: MStmt ==> Set[Asgn] = circular(Set[Asgn]()) {
    case s => assigns(s)
  }

}