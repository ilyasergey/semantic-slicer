package org.semantic.lang.syntax.parsing

import util.parsing.syntax.StdTokens

/**
 * @author ilyas
 */

trait MatlabTokens extends StdTokens {

  /** The class of integer literal tokens */
  case class IntegerLit(chars: String) extends Token {
    override def toString = chars
  }

  /** The class of floating point literal tokens */
  case class FloatLit(chars: String) extends Token {
    override def toString = chars
  }

  
}