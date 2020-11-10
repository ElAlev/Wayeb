package fsm.symbolic.sre

import java.io.FileReader
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Parser for the declarations file.
  * Each declaration line can be:
  *   - Sets of extra predicates to be taken into account when building the SDFA and the Markov chain that do not
  *   appear in the pattern itself but may be important features for forecasting. Extra predicates are given in
  *   parentheses, preceded by +.
  *   For example, if a pattern on moving objects has a single predicate like SpeedGreaterThan(20), we might also want
  *   to add other predicates, like SpeedLowerThan(5) or SpeedBetween(5,20).
  *   We then write +(SpeedLowerThan(5),SpeedBetween(5,20)).
  *   - Sets of mutually exclusive predicates that help in reducing the size of the SDFA. Exclusive predicates are
  *   given in parentheses preceded by ~. From all predicates inside such parentheses at most one may evaluate to TRUE.
  *   For example, we could write ~(SpeedLowerThan(5),SpeedBetween(5,20)). This means that we can avoid creating all
  *   the (4) min-terms between these two predicates. Make sure that the predicates are indeed mutually exclusive,
  *   otherwise unintended semantics may be introduced.
  *
  *   Different sets of extras/exclusives given in separate lines.
  *
  */

trait DeclarationsParser extends JavaTokenParsers with SREParser {

  def exclusive: Parser[Exclusive] = "~(" ~ repsep(logicAtomic, ",") ~ ")" ^^ {
    case "~(" ~ sentences ~ ")" => Exclusive(sentences)
  }

  def extra: Parser[Extras] = "+(" ~ repsep(logicAtomic, ",") ~ ")" ^^ {
    case "+(" ~ sentences ~ ")" => Extras(sentences)
  }

  def declaration: Parser[Declaration] = exclusive | extra

  def declarations: Parser[List[Declaration]] = repsep(declaration, ",") ^^ {
    case decl => decl
  }

}

object ParseDeclarations extends DeclarationsParser {
  def main(args: Array[String]): Unit = {
    val home = System.getenv("WAYEB_HOME")
    val fn = home + "/patterns/sre/test/declarations1.sre"
    val reader = new FileReader(fn)
    val parsed = parseAll(declarations, reader)
    val f = parsed.get
    println(parsed)
    println(f)

  }
}
