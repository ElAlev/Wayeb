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
  * When applied to disjunction or negation, selection strategies have no effect.
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
  *   - The assumed order of the Markov chain, denoted by a positive integer (or zero) inside {}.
  *   - A partition attribute, denoted by a string starting with a lowercase letter inside {}.
  *   - A window, denoted by a positive integer inside {}.
  *   - A window type, denoted by a string, either 'time' or 'count',  inside {}.
  *     To be used only if a window has also been used.
  *
  * If a partition attribute exists, this means that the stream will be split into sub-streams according to the value
  * of the attribute that each event has. For example, in the maritime domain we may need to apply a pattern on a
  * per-vessel basis. We thus designate the vessel's MMSI (id) as the partition attribute. This attribute must exist
  * in all the input events.
  *
  * A window imposes the extra constraint that the 'duration' of every complex event must not exceed the window value.
  * There are two types of windows:
  * - Count-based ones. The 'duration' of a complex event in this case is just the number of its input events.
  *   For a match to be a valid complex event, the number of its input events must not exceed the window value.
  * -Time-based ones. The 'duration' of a complex event is the difference between the timestamps of its last and first
  *  input events. Each input event must have a 'timestamp' attribute.
  *
  *  If no window type is provided, then a count-based window is used. Windows are required only for NSRA and DSRA.
  *
  * A file may contain multiple patterns, separated by &.
  *
  * Full example,
  *
  * ;(IsEventTypePredicate(A),IsEventTypePredicate(B)){order:2}
  * &
  * ;(|(IsEventTypePredicate(A),IsEventTypePredicate(B)),IsEventTypePredicate(C)){order:0}{partitionBy:someAttribute}{window:10}{windowType:count}
  *
  * IsEventTypePredicate just checks for the event type and is already implemented.
  *
  * Note that for each predicate in a pattern, there must exist an implementation with the same name under
  * fsm.symbolic.logic.predicates
  *
  *
  * ADDENDUM for symbolic regular expressions with memory and output (SREMO).
  * SRE can be enriched with register variables in order to write SREMO.
  * Register variables are used in two ways.
  * First, they must be declared. Each sentence may be accompanied at the end by a register variable, provided
  * as a string literal. For example, IsEventTypePredicate(A)["x"] denotes an atomic sentence (IsEventTypePredicate(A))
  * and x is the register variables. Register variables must be declared immediately after a sentence
  * and must be written inside brackets and double quotes ([""]). A register variable declaration implies
  * that the event triggering the sentence must be stored in a register with the corresponding name. A register
  * variable may not be declared with the same name multiple times.
  * Second, a register variable may be referred to in the arguments list of an atomic sentence (other than the one in
  * which it was declared). For example, EQAttrStr(EventType,"x") is an atomic sentence whose second argument is the
  * register variable x. Register variables in an argument list must be provided inside double quotes. The meaning is
  * that the atomic sentence can have access to the contents of this register during its evaluation. For example,
  * EQAttrStr(EventType,"x") evaluates to true iff the event type of the current event is equal to the event type of the
  * event that has been store in the register x. Any predicates handling register variables in their arguments must also
  * have been implemented under fsm.symbolic.logic.predicates.
  *
  * SREMO should also be accompanied by a window, given as a natural integer at the end of the SREMO.
  * For example, ;(IsEventTypePredicate(A)["x"],EQAttrStr(EventType,"x")){order:1}{window:2} is a SREMO with a
  * (count-based) window of length 2. Note that if a window is not provided, it will be set to a default value of 0,
  * which effectively renders the SREMO unsatisfiable.
  */

trait SREParser extends JavaTokenParsers {
  def logicConstant: Parser[LogicConstant] = """[A-Z]\w*""".r ^^ (x => LogicConstant(x))
  def numericalConstant: Parser[NumericalConstant] = """-?\d*\.\d*""".r ^^ (x => NumericalConstant(x.toDouble))
  def logicVariable: Parser[LogicVariable] = """[a-z]\w*""".r ^^ (x => sre.LogicVariable(x))
  def registerVariable: Parser[RegisterVariable] = stringLiteral ^^ (x => sre.RegisterVariable(x)) //"""[a-z]\w*""".r ^^ (x => sre.LogicVariable(x))
  def registerVariableDeclaration: Parser[RegisterVariable] = "[" ~ registerVariable ~ "]" ^^ {
    case "[" ~ x ~ "]" => x
  }
  def logicTerm: Parser[LogicTerm] = logicConstant | logicVariable | registerVariable | numericalConstant
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

  def atomicFormulaPrefix: Parser[SRESentence] = logicSentencePrefix ~ opt(registerVariableDeclaration) ^^ {
    case lsp ~ Some(rv) => SRESentence(lsp, rv)
    case lsp ~ None => SRESentence(lsp)
  }
  def complexFormulaPrefix: Parser[SREOperator] = seqPrefix | orPrefix | iterPrefix | negationPrefix | anyPrefix | nextPrefix
  def formulaPrefix: Parser[SREFormula] = atomicFormulaPrefix | complexFormulaPrefix

  def natInt: Parser[Int] = """\d+""".r ^^ (x => x.toInt)
  def order: Parser[Int] = "{order:" ~ natInt ~ "}" ^^ {
    case "{order:" ~ o ~ "}" => o
  }

  def formulaWithOrder: Parser[(SREFormula, Int)] = formulaPrefix ~ opt(order) ^^ {
    case f ~ Some(o) => {
      if (!checkVariables(f)) throw new IllegalArgumentException("There are non-declared argument variables")
      (f, o)
    }
    case f ~ None => {
      if (!checkVariables(f)) throw new IllegalArgumentException("There are non-declared argument variables")
      (f, 0)
    }
  }

  def partitionAttr: Parser[String] = """[a-z]\w*""".r ^^ (x => x.toString)
  def partition: Parser[String] = "{partitionBy:" ~ partitionAttr ~ "}" ^^ {
    case "{partitionBy:" ~ p ~ "}" => p
  }

  def formulaWithPartition: Parser[(SREFormula, Int, String)] = formulaWithOrder ~ opt(partition) ^^ {
    case (f, o) ~ Some(p) => (f, o, p)
    case (f, o) ~ None => (f, o, "$")
  }

  def window: Parser[Int] = "{window:" ~ natInt ~ "}" ^^ {
    case "{window:" ~ o ~ "}" => o
  }

  def formulaWithWindow: Parser[(SREFormula, Int, String, Int)] = formulaWithPartition ~ opt(window) ^^ {
    case (f, o, p) ~ Some(w) => (f, o, p, w)
    case (f, o, p) ~ None => (f, o, p, 0)
  }

  def windowLiteral: Parser[String] = """count|time""".r ^^ (x => x)
  def windowType: Parser[String] = "{windowType:" ~ windowLiteral ~ "}" ^^ {
    case "{windowType:" ~ p ~ "}" => p match {
      case "count" => "count"
      case "time" => "time"
      case _ => throw new IllegalArgumentException("windowType must be count or time")
    }
  }

  def formulaWithWindowType: Parser[(SREFormula, Int, String, Int, String)] = formulaWithWindow ~ opt(windowType) ^^ {
    case (f, o, p, w) ~ Some(wt) => (f, o, p, w, wt)
    case (f, o, p, w) ~ None => (f, o, p, w, "count")
  }

  def formulasList: Parser[List[(SREFormula, Int, String, Int, String)]] = repsep(formulaWithWindowType, "&") ^^ {
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
        case _ => {
          if (!regVariablesUnique(formulas)) throw new IllegalArgumentException("Register variables may not be repeated")
          SREOperator(RegularOperator.SEQ, formulas)
        }
      }
      case RegularOperator.CHOICE => formulas match {
        case Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ :: Nil => throw new IllegalArgumentException("SEQ operator needs at least two sub-formulas")
        case _ => {
          if (!regVariablesUnique(formulas)) throw new IllegalArgumentException("Register variables may not be repeated")
          SREOperator(RegularOperator.CHOICE, formulas)
        }
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

  /**
   * Checks whether all register variables of a formula referenced as arguments in sentences have also been declared.
   *
   * @param f The formula to check.
   * @return True if there are no "free" register variables, i.e., variables not declared.
   */
  private def checkVariables(f: SREFormula): Boolean = {
    val rv = f.getRegisterVariables.map(v => v.toString)
    f.getVariableArguments.forall(va => rv.contains(va))
  }

  /**
   * Determines whether the register variables from a list of formulas does not contain the same variable multiple times.
   *
   * @param formulas The list of formulas to check.
   * @return True if all register variables are unique, false if a register variable is declared more than once.
   */
  private def regVariablesUnique(formulas: List[SREFormula]): Boolean = {
    val regVars = formulas.flatMap(f => f.getRegisterVariables.toList.map(lv => lv.toString)).filter(rv => rv != "")
    regVars.length == regVars.toSet.size
  }

}

object ParseSREFormula$ extends SREParser {
  def main(args: Array[String]): Unit = {
    val home = System.getenv("WAYEB_HOME")
    val fn = home + "/patterns/validation/pattern2.sre"
    val reader = new FileReader(fn)
    val parsed = parseAll(formulasList, reader)
    val f = parsed.get
    println(parsed)
    println(f)
  }
}
