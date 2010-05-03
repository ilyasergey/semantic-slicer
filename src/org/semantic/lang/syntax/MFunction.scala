package org.semantic.lang.syntax

/**
 * @author ilyas
 *
 * Function definition in MATLAB language
 */

case class MFunction(name: Id, res: Seq[Id], params: Seq[Id])