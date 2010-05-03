package org.semantic.lang.syntax.parsing

import util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * @author ilyas
 */

class MatlabLexical extends StdLexical with MatlabTokens {

  override def whitespace = rep(
    whitespaceChar
    | comment
    )

  override protected def comment = '%' ~ rep(chrExcept(EofCh, '\n'))
}