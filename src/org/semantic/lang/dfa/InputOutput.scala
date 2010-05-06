package org.semantic.lang.dfa

import kiama.attribution.DynamicAttribution._
import org.semantic.lang.syntax._
import kiama.attribution.Attributable


/**
 * @author ilyas
 */

trait InputOutput {
  val inputVariables: Selected ==> Set[Var]

  val outputVariables: Selected ==> Set[Var]
}

trait InputOutputImpl extends InputOutput with VariablesImpl with ControlFlowImpl {
  val inputVariables: Selected ==> Set[Var] = attr {
    case s => s -> uses
  }


  val outputVariables: Selected ==> Set[Var] = attr {
    case sel => succTrans(sel).flatMap(uses(_)) intersect defines(sel)
  }
}