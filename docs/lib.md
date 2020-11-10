# Using Wayeb as a library

You can also use Wayeb as a library.
This is a more flexible way to use Wayeb,
since not all options are exposed to the command line interface.

The general idea is that,
for every task,
we create the structures that we are going to be needing in the form of *providers*.
For example, if we want to perform recognition,
we need to create a SDFA (symbolic deterministic finite automaton) from a pattern.
We can do this by creating a provider for the SDFA.
The reason for wrapping the SDFA inside a provider wrapper is that there are multiple ways
in which a SDFA may be created. 
The standard way is to construct it from a pattern given in a file.
Another way would be to construct it from a pattern given as a regular expression tree
(very useful for unit testing, 
since it is more convenient to randomly generate such trees than randomly generate files).
The provider makes it easy to construct the SDFA by simply calling its `provide` method,
without needing to know exactly how the SDFA is constructed.

If we want to perform forecasting with a full-order Markov model,
we have to use the SDFA provider to create providers for:
* the Markov chain;
* the waiting-time distributions for each automaton state;
* and finally for the forecast itnervals.

With all these providers available,
we can then run the forecasting task.

See the following example:
```scala
import model.waitingTime.ForecastMethod
import stream.StreamFactory
import ui.ConfigUtils
import workflow.provider.source.forecaster.ForecasterSourceBuild
import workflow.provider.source.matrix.MCSourceMLE
import workflow.provider.source.sdfa.SDFASourceFromSRE
import workflow.provider.source.wt.{WtSourceDirect, WtSourceMatrix}
import workflow.provider._
import workflow.task.engineTask.ERFTask

  val confidenceThreshold = 0.5
  val horizon = 50
  val domain = "maritime"
  val maxSpread = 5
  val method = ForecastMethod.CLASSIFY_NEXTK
  val distance = (0.0001, 1.0)
  val home = System.getenv("WAYEB_HOME")
  val dataDir: String = home + "/data/maritime/"
  val resultsDir: String = home + "/results"
  val testDatasetFilename: String = dataDir + "227592820.csv"
  val trainDatasetFilename: String = dataDir + "227592820.csv"
  val patternFile: String = home + "/patterns/maritime/port/pattern.sre"
  val declarationsFile: String = home + "/patterns/maritime/port/declarationsDistance1.sre"

  // First create the training and test stream sources.
  // For convenience, here we use the same file, but should be different in real experiments.
  val streamTrainSource = StreamFactory.getDomainStreamSource(trainDatasetFilename, domain = domain, List.empty)
  val streamTestSource = StreamFactory.getDomainStreamSource(testDatasetFilename, domain = domain, List.empty)

  // Create a provider for the SDFAval sdfap = SDFAProvider(SDFASourceFromSRE(patternFile, ConfigUtils.defaultPolicy, declarationsFile))
  val sdfap = SDFAProvider(SDFASourceFromSRE(patternFile, ConfigUtils.defaultPolicy, declarationsFile))
  // Wrap a FSM provider around it
  val fsmp = FSMProvider(sdfap)
  // Create a provider for the Markov chain model
  val mp = MarkovChainProvider(MCSourceMLE(fsmp, streamTrainSource))
  // Create a provider for the waiting-time distributions
  val wtp = WtProvider(WtSourceMatrix(fsmp, mp, horizon = horizon, finalsEnabled = false))
  // Create a provider for the forecast intervals
  val pp = ForecasterProvider(ForecasterSourceBuild(
    fsmp,
    wtp,
    horizon             = horizon,
    confidenceThreshold = confidenceThreshold,
    maxSpread           = maxSpread,
    method              = ForecastMethod.CLASSIFY_NEXTK
  ))

  // Now execute recognition and forecasting
  val erft = ERFTask(
    fsmp             = fsmp,
    pp               = pp,
    predictorEnabled = true,
    finalsEnabled    = false,
    expirationDeadline   = ConfigUtils.defaultExpiration,
    distance         = distance,
    streamSource     = streamTestSource,
    collectStats = true,
    show = true
  )
  val prof = erft.execute()
  prof.printProfileInfo()

  val f1score = prof.getStatFor("f1", 0)
  println("\n\n\n\n\n\tF1-score: " + f1score)
```

Note that the `provide` method of the providers is actally called when the `ERFTask` is executed.
This means that the SDFA is constructed at this point. 
This may be inefficient if you want to run forecasting with the same SDFA,
but with different parameter values (e.g., a difference confidence threshold).
The SDFA will be constructed again when the new `ERFTask` is executed.
In order to avoid this overhead,
you can actually call the `provide` method as soon as you have created the first provider
(whose source is tha pattern file).
This will create the SDFA at this point.
You can then wrap this SDFA in another provider with the source being this SDFA itself.  

A similar process is followed for variable-order Markov models.
See also the script [ui.demo.RunSrc](cef/src/main/scala/ui/demo/RunSrc.scala).