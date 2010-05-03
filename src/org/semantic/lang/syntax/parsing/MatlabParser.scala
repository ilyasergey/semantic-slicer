package org.semantic.lang.syntax.parsing

import util.parsing.combinator.syntactical.StandardTokenParsers
import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

class MatlabParser extends StandardTokenParsers {
  override val lexical = new MatlabLexical
  import lexical._

  lexical.delimiters += ("(", ")", "=", ".", "[", "]", ";",
          ",", "*", "/", "+", "-", "^")
  lexical.reserved += ("function", "for", "end", "if",
          "else", "elseif", "continue", "while", "break")

  def program: Parser[MSeq] = (
  seq        
  )

  def seq: Parser[MSeq] = repsep(stmt, separators) ^^ {case List(stmts @ _*) => MSeq(stmts)}

  def separators = rep1(";") 

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
    addExpr
  )

  def addExpr: Parser[MExp] = multExpr ~ rep(("+" | "-") ~ multExpr) ^^ {case s ~ l =>
    l.foldLeft(s)((x, y) => y match {
      case "+" ~ e => Add(x, e)
      case "-" ~ e => Sub(x, e)
    })
  }

  def multExpr: Parser[MExp] = powerExpr ~ rep(("*" | "/") ~ powerExpr) ^^ {case s ~ l =>
    l.foldLeft(s)((x, y) => y match {
      case "*" ~ e => Mul(x, e)
      case "/" ~ e => Div(x, e)
    })
  }

  def powerExpr: Parser[MExp] = simpleExpr ~ rep("^" ~ simpleExpr) ^^ {case s ~ l =>
      (s /: l)((x, y) => y match {case "^" ~ e => Pow(x,e)})}

  def simpleExpr: Parser[MExp] = (
    ident ^^ Var
  | floatLiteral
  | intLiteral
  | "(" ~> expr <~ ")"
  | stringLit ^^ MString
  )

  def intLiteral: Parser[IntNum] =
    elem("integer", _.isInstanceOf[IntegerLit]) ^^ (x => IntNum(Integer.parseInt(x.chars)))

  def floatLiteral: Parser[FloatNum] =
    elem("float", _.isInstanceOf[FloatLit]) ^^ (x => FloatNum(java.lang.Float.parseFloat(x.chars)))



  // Scanner is imported from STLCLexer, which is an inheritor of Scanners
  def parse(input: String): ParseResult[MSeq] = program(new lexical.Scanner(input)) match {
    case _ : Error => Failure("unknown failure", new lexical.Scanner(input))
    case Success(_, in) if !in.atEnd => Failure("Non fully parsed", in)
    case s => s
  }


}