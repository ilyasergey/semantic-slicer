package org.semantic.lang.syntax.parsing

import util.parsing.combinator.syntactical.StandardTokenParsers
import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

class MatlabParser extends StandardTokenParsers {
  override val lexical = new MatlabLexical

  lexical.delimiters += ("(", ")", "=", ".", "[", "]", ";", ",")
  lexical.reserved += ("function", "for", "end", "if",
          "else", "elseif", "continue", "while", "break")

  def program: Parser[MSeq] = (
  seq        
  )

  def seq: Parser[MSeq] = repsep(stmt, ";") ^^ {case List(stmts @ _*) => MSeq(stmts)}

  def stmt : Parser[MStmt] = (
    ident ~ expr ^^ {case i ~ e => Asgn(Id(i), e)}
  | ("if" ~> expr <~ opt(",")) ~ seq ~ rep("elseif" ~> seq) ~ opt("else" ~> seq) <~ "end" ^^ {
      case cond ~ tb ~ List(elseIfBranches @ _*) ~ elseBranch => {
        val lastElseBranch = elseBranch match {
          case Some(eb) => Seq(eb)
          case None => Seq()
        }
        IfStmt(cond, tb, elseIfBranches ++ lastElseBranch)
      }
    }
  | expr
  )

  def expr: Parser[MExp] = (
    simpleExpr
  )

  def simpleExpr: Parser[MExp] = (
    ident ^^ Var
  | "(" ~> expr <~ ")"
  //  | stringLit ^^ StringLiteral
  )


  // Scanner is imported from STLCLexer, which is an inheritor of Scanners
  def parse(input: String): ParseResult[MSeq] = program(new lexical.Scanner(input)) match {
    case _ : Error => Failure("unknown failure", new lexical.Scanner(input))
    case Success(_, in) if !in.atEnd => Failure("Non fully parsed", in)
    case s => s
  }


}