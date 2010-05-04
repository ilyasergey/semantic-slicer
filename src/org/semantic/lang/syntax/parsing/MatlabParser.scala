package org.semantic.lang.syntax.parsing

import util.parsing.combinator.syntactical.StandardTokenParsers
import org.semantic.lang.syntax._

/**
 * @author ilyas
 */

class MatlabParser extends StandardTokenParsers {
  override val lexical = new MatlabLexical
  import lexical._

  lexical.delimiters += ("(", ")", "=", ".", "[", "]", ";", ":", "||", "&&",
          ",", "*", "/", "+", "-", "^", "==", "<=", ">=", ">", "<", "\n", "\r")

  lexical.reserved += ("function", "for", "end", "if",
          "else", "elseif", "continue", "while", "break", "return", "break", "continue")

  def program: Parser[MStmt] = (
    opt(separators) ~> seq <~ opt(separators)
  )

  /**
   * Sequence of statements
   */
  def seq: Parser[MStmt] = stmt ~ rep(separators ~> stmt) <~ opt(separators) ^^ {case s ~ l => l match {
    case List() => s
    case x => MSeq(s :: x)
  }}

  def separators: Parser[Any] = rep1(";" | newline)  

  def newline: Parser[Any] = rep1("\n" | "\r")
  def optnl: Parser[Any] = opt(newline)

  def asgn: Parser[Asgn] = (ident <~ "=") ~ expr ^^ {case i ~ e => Asgn(Id(i), e)}

  def stmt : Parser[MStmt] = (
      asgn
  ||| ("if" ~> expr <~ opt(",") <~ optnl) ~ seq ~ rep(("elseif" ~> optnl ~> expr <~ opt(",") <~ optnl) ~ seq) ~
              opt("else" ~> optnl ~> seq) <~ "end" ^^ {
        case cond ~ tb ~ thenBranches ~ elseBranch => {
          val tbx = (cond, tb) :: (thenBranches map {case x ~ y => (x,y)})
          IfStmt(tbx, elseBranch)
        }

      }
  ||| ("for" ~> optnl ~> asgn <~ opt(",") <~ optnl) ~ seq <~ "end"  ^^ {case a ~ s => ForStmt(a, s)}
  ||| ("while" ~> optnl ~> expr <~ opt(",") <~ optnl) ~ seq <~ "end"  ^^ {case a ~ s => While(a, s)}
  ||| "return" ^^^ Return
  ||| "break" ^^^ Break
  ||| "continue" ^^^ Continue
  ||| expr
  )

  def expr: Parser[MExp] = logicExpr

  def logicExpr: Parser[MExp] = {
    def conj = equalExpr ~ rep("&&" ~> optnl ~> equalExpr) ^^ {case s ~ l => (s /: l)(Conj)}
    conj ~ rep("||" ~> optnl ~> conj) ^^ {case s ~ l => (s /: l)(Disj)}
  }

  def equalExpr: Parser[MExp] = rangeExpr ~ rep((("==" | ">=" | "<=" | "<" | ">") <~ optnl) ~ rangeExpr) ^^ {case s ~ l =>
    (s /: l)((x, y) => y match {
      case _ ~ e => Equal(x, e)
    })
  }

  def rangeExpr: Parser[MExp] = (addExpr
  ||| (addExpr <~ ":" <~ optnl) ~ addExpr ^^ {case e1 ~ e2 => Range(e1, e2)}
  )

  def addExpr: Parser[MExp] = multExpr ~ rep((("+" | "-") <~ optnl) ~ multExpr) ^^ {case s ~ l =>
    l.foldLeft(s)((x, y) => y match {
      case "+" ~ e => Add(x, e)
      case "-" ~ e => Sub(x, e)
    })
  }

  def multExpr: Parser[MExp] = powerExpr ~ rep((("*" | "/") <~ optnl) ~ powerExpr) ^^ {case s ~ l =>
    l.foldLeft(s)((x, y) => y match {
      case "*" ~ e => Mul(x, e)
      case "/" ~ e => Div(x, e)
    })
  }

  def powerExpr: Parser[MExp] = transpExpr ~ rep(("^" <~ optnl) ~ transpExpr) ^^ {case s ~ l =>
      (s /: l)((x, y) => y match {case "^" ~ e => Pow(x,e)})}

  //todo implement transposition via lexer substitution
  def transpExpr: Parser[MExp] = simpleExpr/*.flatMap {e => {
      val quoteParser = new MatlabParser {
        override val lexical = new MatlabLexical {
          override def token = '\'' | failure("illegal character")
        }

        def parseQuote: Parser[MExp]

      }

    }}*/



  def simpleExpr: Parser[MExp] = (
    ident ~ ("(" ~> optnl ~> repsep(expr, optnl ~> "," <~ optnl) <~ optnl <~ ")") ^^ {case n ~ args => MCall(Var(n), args)}
  | floatLiteral
  | intLiteral
  | "(" ~> expr <~ ")"
  | stringLit ^^ MString
  | ident ^^ Var
  )

  def intLiteral: Parser[IntNum] =
    elem("integer", _.isInstanceOf[IntegerLit]) ^^ (x => IntNum(Integer.parseInt(x.chars)))

  def floatLiteral: Parser[FloatNum] =
    elem("float", _.isInstanceOf[FloatLit]) ^^ (x => FloatNum(java.lang.Float.parseFloat(x.chars)))



  // Scanner is imported from STLCLexer, which is an inheritor of Scanners
  def parse(input: String): ParseResult[MStmt] = program(new lexical.Scanner(input)) match {
    case _ : Error => Failure("unknown failure", new lexical.Scanner(input))
    case Success(_, in) if !in.atEnd => Failure("Non fully parsed", in)
    case s => s
  }


}