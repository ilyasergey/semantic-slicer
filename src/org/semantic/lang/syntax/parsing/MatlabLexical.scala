package org.semantic.lang.syntax.parsing

import util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh

/**
 * @author ilyas
 */

class MatlabLexical extends StdLexical with MatlabTokens {

  override def token: Parser[Token] =
    ( letter ~ rep( letter | digit )                    ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | rep(digit) ~ '.' ~ rep(digit)                     ^^ { case first ~ '.' ~ rest => FloatLit(first ++ List('.') ++ rest mkString "")}
    | digit ~ rep( digit )                              ^^ { case first ~ rest => IntegerLit(first :: rest mkString "") }
    | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | delim
    | failure("illegal character")
    )


  override def whitespace = rep(
    whitespaceChar
    | comment
    )

  override protected def comment = '%' ~ rep(chrExcept(EofCh, '\n'))
}