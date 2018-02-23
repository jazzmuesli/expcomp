import ComplexImplicits._
import org.junit.Test

class ExplainableComputationsTest {
  @Test
  def testPie() {
    val pi: NumericExpression = Number(Math.PI)
    println("pi=" + pi)
    val e = Number(Math.E)
    println("e=" + e)
    val pie: NumericExpression = pi + e
    println("pie=" + pie)
    val roundedPie: NumericExpression = pie.round(3)
    println("roundedPie=" + roundedPie)
    assert(5.86== roundedPie.getResult)
    /*
pi=3.141592653589793
e=2.718281828459045
pie=(5.859874482048838=3.141592653589793 + 2.718281828459045)
roundedPie=(5.86=(5.859874482048838=3.141592653589793 + 2.718281828459045) round 3.0)
     */
  }
  val EXPECTED_BID: Double = 1.4705
  val EXPECTED_OFFER: Double = 1.6751
  val EURUSD_BID=1.2333
  val EURUSD_OFFER=1.2338
  val AUDUSD_MID=0.7843
  val AUDUSD_SPREAD=0.002

  private val EXTRA_SPREAD: Double = 0.2

  @Test
  def testEURAUD() {
    GlobalExplanationSettings.explain=true
    val EURUSD_SPOT = new Rate(EURUSD_BID, EURUSD_OFFER)
    val AUDUSD_SPOT = new SpreadedRate(AUDUSD_MID, AUDUSD_SPREAD)
    val EURUSD_1M = new Rate(0.0013, 0.0017)
    val AUDUSD_1M = new Rate(0.0023, 0.0029)

    def calculateOutright(spot: Rate, points: Rate) = new Rate(spot.getBid + points.getBid, spot.getOffer + points.getOffer)
    val AUDUSD_OUTRIGHT_1M = calculateOutright(AUDUSD_SPOT,AUDUSD_1M)
    val EURUSD_OUTRIGHT_1M = calculateOutright(EURUSD_SPOT,EURUSD_1M)

    val EURAUD_SPOT = EURUSD_SPOT.cross(AUDUSD_SPOT)

    val EURAUD_OUTRIGHT_1M = EURUSD_OUTRIGHT_1M.cross(AUDUSD_OUTRIGHT_1M)
    val EURAUD_POINTS = new Rate(EURAUD_OUTRIGHT_1M.getBid-EURAUD_SPOT.getBid, EURAUD_OUTRIGHT_1M.getOffer-EURAUD_SPOT.getOffer)
    val extraSpread=0.00049
    val spreadWidenedEURAUD = new SpreadedRate(EURAUD_POINTS.calculateMid,EURAUD_POINTS.spread + extraSpread)
    val roundedEURAUD: Rate = spreadWidenedEURAUD.round(5)

    println("EURUSD=" + EURUSD_SPOT)
    println("AUDUSD=" + AUDUSD_SPOT.prettyPrint)
    println("EURAUD points with spread widened by " + extraSpread + "=" + roundedEURAUD.prettyPrint)
    assert(EXPECTED_BID==roundedEURAUD.getBid.getResult)
    assert(EXPECTED_OFFER==roundedEURAUD.getOffer.getResult)
    /*
EURUSD=Rate(bid=1.2333, offer=1.2338)
AUDUSD=Rate(bid=(0.7833=0.7843 - (0.001=0.002 * 0.5)), offer=(0.7853=0.7843 + (0.001=0.002 * 0.5)))=SpreadedRate(mid=0.7843, spread=0.002)
EURAUD=Rate(bid=(1.5705=(1.5704826181077296=1.2333 / (0.7853=0.7843 + (0.001=0.002 * 0.5))) round 4.0), offer=(1.5751=(1.575130856632197=1.2338 / (0.7833=0.7843 - (0.001=0.002 * 0.5))) round 4.0))
     */
  }

  def perform(explain: Boolean, f: => Unit): Long = {
    GlobalExplanationSettings.explain = explain
    val stime = System.currentTimeMillis()
    for (i <- 1 to 10000000) {
      f
    }
    val duration = System.currentTimeMillis() - stime
    duration
  }

  def performExplainableComputation: Unit = {
    val EURUSD = new Rate(EURUSD_BID, EURUSD_OFFER)
    val AUDUSD = new SpreadedRate(AUDUSD_MID, AUDUSD_SPREAD)
    val EURAUD = EURUSD.cross(AUDUSD)
    val spreadWidenedEURAUD = new SpreadedRate(EURAUD.calculateMid,EURAUD.spread + EXTRA_SPREAD)
    val roundedEURAUD: Rate = spreadWidenedEURAUD.round(4)
    assert(EXPECTED_BID==roundedEURAUD.getBid.getResult)
    assert(EXPECTED_OFFER==roundedEURAUD.getOffer.getResult)
  }

  val rounder4dp: (Double) => Double = MathUtils.roundAt(4)
  def performBespokeComputation = {
    val AUDUSD_BID=AUDUSD_MID-AUDUSD_SPREAD*0.5
    val AUDUSD_OFFER=AUDUSD_MID+AUDUSD_SPREAD*0.5
    val EURAUD_BID=EURUSD_BID/AUDUSD_OFFER
    val EURAUD_OFFER=EURUSD_OFFER/AUDUSD_BID
    val EURAUD_MID=(EURAUD_BID+EURAUD_OFFER)*0.5
    // add extra spread
    val spread = (EURAUD_OFFER-EURAUD_BID)+EXTRA_SPREAD
    val EURAUD_BID_WIDENED = EURAUD_MID-spread*0.5
    val EURAUD_OFFER_WIDENED = EURAUD_MID+spread*0.5
    val EURAUD_BID_ROUNDED=rounder4dp(EURAUD_BID_WIDENED)
    val EURAUD_OFFER_ROUNDED=rounder4dp(EURAUD_OFFER_WIDENED)
    assert(EXPECTED_BID==EURAUD_BID_ROUNDED)
    assert(EXPECTED_OFFER==EURAUD_OFFER_ROUNDED)
  }

  @Test
  def testPerformance(): Unit = {
    val results = Range(0,10).map { i =>
      val durationWithoutExplain = perform(false, performExplainableComputation)
      println("[" + i + "]Duration with explain disabled: " + durationWithoutExplain)
      val durationWithExplain = perform(true, performExplainableComputation)
      println("[" + i + "]Duration with exlain enabled: " + durationWithExplain)
      val durationBespoke= perform(false, performBespokeComputation)
      println("[" + i + "]Duration for bespoke computation: " + durationBespoke)
      Tuple3(durationWithoutExplain, durationWithExplain,durationBespoke)
    }
    //TODO: calculate median, percentiles, etc.
    //TODO: do z-test
    //Average without explanation: 3655
    //Average with explanation: 4155
    //Average bespoke: 70
    // Well, it's _just_ 15-60x slower
    def average(xs: Seq[Long]) = xs.foldLeft(0L)(_+_) / xs.size
    println("Average without explanation: " + average(results.map(x => x._1)))
    println("Average with explanation: " + average(results.map(x => x._2)))
    println("Average bespoke: " + average(results.map(x => x._3)))
  }
}