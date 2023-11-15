package Specs.vmm

import com.typesafe.scalalogging.LazyLogging
import fsm.symbolic.logic.{AtomicSentence, PredicateConstructor, Sentence}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatestplus.junit.JUnitRunner
import stream.StreamFactory
import model.vmm.Symbol
import model.vmm.mapper.Isomorphism
import model.vmm.pst.{CSTLearner, PSTLearner, PredictionSuffixTree, SymbolDistribution}

@RunWith(classOf[JUnitRunner])
class PST extends FlatSpec with LazyLogging {

  /**
    * @article{DBLP:journals/jair/BegleiterEY04,
    *                                           author    = {Ron Begleiter and
    *                                           Ran El{-}Yaniv and
    *                                           Golan Yona},
    *                                           title     = {On Prediction Using Variable Order Markov Models},
    *                                           journal   = {J. Artif. Intell. Res.},
    *                                           volume    = {22},
    *                                           pages     = {385--421},
    *                                           year      = {2004}
    *                                           }
    */
  "Prediction suffix tree learnt on ABRACADABRA " should " be the same as the that of Fig.5 in Begleiter paper " in {
    testABRA()
  }

  def testABRA(): Unit = {
    logger.debug("\n\n\t ABRACADABRA test for PST\n\n")
    val alphabet: List[String] = List("A", "B", "C", "D", "R")
    val symbols = (1 to alphabet.length).map(i => Symbol(i)).toList
    val simpleSentences = alphabet.map(s => AtomicSentence(PredicateConstructor.getEventTypePred(s)).asInstanceOf[Sentence])
    val sentences = simpleSentences
    val iso = Isomorphism(sentences, symbols)
    val str = List("A", "B", "R", "A", "C", "A", "D", "A", "B", "R", "A")
    val trainStream = StreamFactory.getStreamSource(str)
    val maxOrder = 12
    val partitionAttribute = "$"
    val cstLearner = new CSTLearner(maxOrder, iso, partitionAttribute)
    trainStream.emitEventsToListener(cstLearner)
    val cst = cstLearner.getCST
    //logger.info("Done with learning counter suffix tree")
    //logger.info(cst.toString)
    //logger.info("ISO: " + cst.getSymbols.map(s => (s,iso.getMinTermForSymbol(s))).toList )
    //cst.print(iso)
    val pMin = 0.001
    val alpha = 0.01
    val gamma = 0.001
    val r = 1.05
    val psTLearner = PSTLearner(symbols.toSet, maxOrder, pMin, alpha, gamma, r)
    val pst = psTLearner.learnVariant(cst, withMissing = false)
    //logger.info("Done with learning counter suffix tree")
    //logger.info(pst.toString)
    //pst.print(iso,0.00)
    val asymbol = symbols(0)
    val bsymbol = symbols(1)
    val csymbol = symbols(2)
    val dsymbol = symbols(3)
    val rsymbol = symbols(4)

    val canodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.001, bsymbol -> 0.001, csymbol -> 0.001, dsymbol -> 0.996, rsymbol -> 0.001))
    val canode = PredictionSuffixTree(List(asymbol, csymbol), canodeDist)

    val danodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.001, bsymbol -> 0.996, csymbol -> 0.001, dsymbol -> 0.001, rsymbol -> 0.001))
    val danode = PredictionSuffixTree(List(asymbol, dsymbol), danodeDist)

    val ranodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.001, bsymbol -> 0.001, csymbol -> 0.996, dsymbol -> 0.001, rsymbol -> 0.001))
    val ranode = PredictionSuffixTree(List(asymbol, rsymbol), ranodeDist)

    val anodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.001, bsymbol -> 0.498, csymbol -> 0.25, dsymbol -> 0.25, rsymbol -> 0.001))
    val anode = PredictionSuffixTree(List(asymbol), anodeDist, Map[Symbol, PredictionSuffixTree](csymbol -> canode, dsymbol -> danode, rsymbol -> ranode))

    val bnodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.001, bsymbol -> 0.001, csymbol -> 0.001, dsymbol -> 0.001, rsymbol -> 0.996))
    val bnode = PredictionSuffixTree(List(bsymbol), bnodeDist)

    val cnodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.996, bsymbol -> 0.001, csymbol -> 0.001, dsymbol -> 0.001, rsymbol -> 0.001))
    val cnode = PredictionSuffixTree(List(csymbol), cnodeDist)

    val dnodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.996, bsymbol -> 0.001, csymbol -> 0.001, dsymbol -> 0.001, rsymbol -> 0.001))
    val dnode = PredictionSuffixTree(List(dsymbol), dnodeDist)

    val rnodeDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.996, bsymbol -> 0.001, csymbol -> 0.001, dsymbol -> 0.001, rsymbol -> 0.001))
    val rnode = PredictionSuffixTree(List(rsymbol), rnodeDist)

    val epsilonDist = SymbolDistribution(Map[Symbol, Double](asymbol -> 0.45, bsymbol -> 0.183, csymbol -> 0.092, dsymbol -> 0.092, rsymbol -> 0.183))
    val root = PredictionSuffixTree(List.empty, epsilonDist, Map[Symbol, PredictionSuffixTree](asymbol -> anode, bsymbol -> bnode, csymbol -> cnode, dsymbol -> dnode, rsymbol -> rnode))

    val cmp = root.compare(pst, 0.005)
    assert(cmp)

  }

  /*def testStream(): Unit = {
    logger.debug("\n\n\t Consistency test for PST\n\n")
    val stream = SymbolWordGenerator.generateSymbolStream(3, ConfigUtils.symbolStreamSize)
    val k = 1
    val buf = new CyclicBuffer(k + 1)
    val cst = CounterSuffixTree()
    var symbols: Set[Symbol] = Set.empty
    for (i <- stream.indices) {
      buf.pushSymbol(stream(i))
      val bufferedWord = buf.pop
      cst.updateWithNewWord(bufferedWord)
      symbols = symbols + stream(i)
    }
    val pstl = PSTLearner(symbols, k, ConfigUtils.maxNoStates)
    val pst = pstl.learnOriginal(cst, true)
  }*/

}
