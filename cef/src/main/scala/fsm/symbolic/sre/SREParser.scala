package fsm.symbolic.sre

import scala.util.parsing.combinator._
import java.io.FileReader
import fsm.symbolic.sre
import fsm.symbolic.sre.RegularOperator.RegularOperator

/**
  * Parser for compiling a list of patterns into formulas.
  *
  * Each pattern is a symbolic regular expression, i.e., it is composed of a regular part and a logic part.
  *
  * The operators for the regular part are those of standard regular expressions:
  *   - concatenation/sequence denoted by ;
  *   - disjunction denoted by +
  *   - Kleene-star/iteration denoted by *
  * Regular expressions are written using prefix notation. For example, ;(a,*(+(b,c)),d) means an a, followed by
  * zero or more bs or cs followed by a d.
  * Negation also available, denoted by !.
  * Three selection strategies available:
  *   - Strict contiguity. Default strategy, does not have a symbol.
  *   - Skip-till-any, denoted by #.
  *   - Skip-till-next, denoted by @.
  * When applied to disjunction or negation, selection strategies have not effect.
  *
  * In symbolic regular expressions, terminal symbols (e.g., a,b,c, etc.) are replaced with Boolean expressions.
  * A Boolean expression is either an atomic or a complex sentence.
  * An atomic sentence is a predicate, possibly with arguments and each argument is a term.
  * Predicate names are strings starting with uppercase letters, e.g., ThisIsAPredicate.
  * Terms may be:
  *   - Logic Constants, as strings starting with uppercase letters, e.g., ThisIsAConstant.
  *   - Numerical Constants, as (possibly negated) decimals, e.g. 4.0 or -13.666.
  *   - Variables, as strings starting with lowercase letters, e.g., thisIsAVariable.
  * Note that variables should not be used currently. To be supported in future versions.
  * A complex sentence is composed of atomic sentences combined through the logical operations of:
  *   - Conjunction/AND, denoted by
  *   - Disjunction/OR, denoted by |
  *   - Negation, denoted by -
  * Again, prefix notation is used.
  * For example, if P and Q are predicates, then |(P,-Q) is a complex sentence (P or not Q).
  *
  * Each pattern may be followed by:
  *   - The assumed order of the Markov chain, denoted by a positive integer (or zero) inside {}
  *   - A partition attribute, denoted by a string starting with a lowercase letter inside {}
  *
  * A file may contain multiple patterns, separated by &.
  *
  * Full example,
  *
  * ;(IsEventTypePredicate(A),IsEventTypePredicate(B)){order:2}
  * &
  * ;(|(IsEventTypePredicate(A),IsEventTypePredicate(B)),IsEventTypePredicate(C)){order:0}{partitionBy:someAttribute}
  *
  * IsEventTypePredicate just checks for the event type and is already implemented.
  *
  * Note that for each predicate in a pattern, there must exist an implementation with the same name under
  * fsm.symbolic.sfa.logic.predicates
  */

trait SREParser extends JavaTokenParsers {
  def logicConstant: Parser[LogicConstant] = """[A-Z]\w*""".r ^^ (x => LogicConstant(x))
  def numericalConstant: Parser[NumericalConstant] = """-?\d*\.\d*""".r ^^ (x => NumericalConstant(x.toDouble))
  def logicVariable: Parser[LogicVariable] = """[a-z]\w*""".r ^^ (x => sre.LogicVariable(x))
  def logicTerm: Parser[LogicTerm] = logicConstant | logicVariable | numericalConstant
  def logicPredicate: Parser[LogicPredicate] = """[A-Z]\w*""".r ^^ (x => LogicPredicate(x))
  def logicPredicateArgs: Parser[List[LogicTerm]] = "(" ~ repsep(logicTerm, ",") ~ ")" ^^ {
    case "(" ~ terms ~ ")" => terms
  }
  def logicAtomic: Parser[LogicAtomicSentence] = logicPredicate ~ opt(logicPredicateArgs) ^^ {
    case p ~ Some(t) => LogicAtomicSentence(p, t)
    case p ~ None => LogicAtomicSentence(p, List.empty[LogicTerm])
  }

  // Logic operators
  def logicAndPrefix: Parser[LogicComplexSentence] = "^(" ~ repsep(logicSentencePrefix, ",") ~ ")" ^^ {
    case "^(" ~ sentences ~ ")" => LogicComplexSentence(BooleanOperator.AND, sentences)
  }
  def logicOrPrefix: Parser[LogicComplexSentence] = "|(" ~ repsep(logicSentencePrefix, ",") ~ ")" ^^ {
    case "|(" ~ sentences ~ ")" => LogicComplexSentence(BooleanOperator.OR, sentences)
  }
  def logicNegated: Parser[LogicComplexSentence] = "-" ~ logicSentencePrefix ^^ {
    case "-" ~ s => LogicComplexSentence(BooleanOperator.NOT, List(s))
  }

  def logicComplexPrefix: Parser[LogicSentence] = logicNegated | logicAndPrefix | logicOrPrefix

  def logicSentencePrefix: Parser[LogicSentence] = logicAtomic | logicComplexPrefix

  // Regular expression operators
  def seqPrefix: Parser[SREOperator] = ";(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case ";(" ~ formulas ~ ")" => parseFormulas(RegularOperator.SEQ, formulas)
  }
  def orPrefix: Parser[SREOperator] = "+(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case "+(" ~ formulas ~ ")" => parseFormulas(RegularOperator.CHOICE, formulas)
  }
  def iterPrefix: Parser[SREOperator] = "*(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case "*(" ~ formulas ~ ")" => parseFormulas(RegularOperator.ITER, formulas)
  }
  def negationPrefix: Parser[SREOperator] = "!(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case "!(" ~ formulas ~ ")" => parseFormulas(RegularOperator.NEG, formulas)
  }
  // Selection strategies as operators
  def anyPrefix: Parser[SREOperator] = "#(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case "#(" ~ formulas ~ ")" => parseFormulas(RegularOperator.ANY, formulas)
  }
  def nextPrefix: Parser[SREOperator] = "@(" ~ repsep(formulaPrefix, ",") ~ ")" ^^ {
    case "@(" ~ formulas ~ ")" => parseFormulas(RegularOperator.NEXT, formulas)
  }

  def atomicFormulaPrefix: Parser[SRESentence] = logicSentencePrefix ^^ (x => SRESentence(x))
  def complexFormulaPrefix: Parser[SREOperator] = seqPrefix | orPrefix | iterPrefix | negationPrefix | anyPrefix | nextPrefix
  def formulaPrefix: Parser[SREFormula] = atomicFormulaPrefix | complexFormulaPrefix

  def natInt: Parser[Int] = """\d+""".r ^^ (x => x.toInt)
  def order: Parser[Int] = "{order:" ~ natInt ~ "}" ^^ {
    case "{order:" ~ o ~ "}" => o
  }

  def formulaWithOrder: Parser[(SREFormula, Int)] = formulaPrefix ~ opt(order) ^^ {
    case f ~ Some(o) => (f, o)
    case f ~ None => (f, 0)
  }

  def partitionAttr: Parser[String] = """[a-z]\w*""".r ^^ (x => x.toString)
  def partition: Parser[String] = "{partitionBy:" ~ partitionAttr ~ "}" ^^ {
    case "{partitionBy:" ~ p ~ "}" => p
  }

  def formulaWithPartition: Parser[(SREFormula, Int, String)] = formulaWithOrder ~ opt(partition) ^^ {
    case (f, o) ~ Some(p) => (f, o, p)
    case (f, o) ~ None => (f, o, "$")
  }

  def formulasList: Parser[List[(SREFormula, Int, String)]] = repsep(formulaWithPartition, "&") ^^ {
    case fl => fl
  }

  def parseFormulas(
                     op: RegularOperator,
                     formulas: List[SREFormula]
                   ): SREOperator = {
    op match {
      case RegularOperator.ITER => SREOperator(RegularOperator.ITER, formulas)
      case RegularOperator.NEG => SREOperator(RegularOperator.NEG, formulas)
      case RegularOperator.SEQ => formulas match {
        case Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ :: Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ => SREOperator(RegularOperator.SEQ, formulas)
      }
      case RegularOperator.CHOICE => formulas match {
        case Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ :: Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ => SREOperator(RegularOperator.CHOICE, formulas)
      }
      case RegularOperator.ANY => formulas match {
        case Nil => throw new IllegalArgumentException("ANY operator takes only a single sub-formula")
        case head :: Nil => SREOperator(RegularOperator.ANY, List(head))
        case _ :: _ :: _ => throw new IllegalArgumentException("ANY operator takes only a single sub-formula")
      }
      case RegularOperator.NEXT => formulas match {
        case Nil => throw new IllegalArgumentException("NEXT operator takes only a single sub-formula")
        case head :: Nil => SREOperator(RegularOperator.NEXT, List(head))
        case _ :: _ :: _ => throw new IllegalArgumentException("NEXT operator takes only a single sub-formula")
      }
    }
  }

}

object ParseSREFormula$ extends SREParser {
  def main(args: Array[String]): Unit = {
    val home = System.getenv("WAYEB_HOME")
    val fn = home + "/patterns/maritime/port/pattern.sre"
    val reader = new FileReader(fn)
    val parsed = parseAll(formulasList, reader)
    val f = parsed.get
    println(parsed)
    println(f)

  }
}
