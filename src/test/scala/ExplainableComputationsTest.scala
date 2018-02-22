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

  @Test
  def testEURAUD() {
    val EURUSD = new Rate(1.2333, 1.2338)
    println("EURUSD=" + EURUSD)
    val AUDUSD = new SpreadedRate(0.7843, 0.002)
    println("AUDUSD=" + AUDUSD)
    val EURAUD = EURUSD.cross(AUDUSD)
    val roundedEURAUD: Rate = EURAUD.round(4)
    println("EURAUD=" + roundedEURAUD)
    assert(1.5705==roundedEURAUD.getBid.getResult)
    assert(1.5751==roundedEURAUD.getOffer.getResult)
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
    val EURUSD = new Rate(1.2333, 1.2338)
    val AUDUSD = new SpreadedRate(0.7843, 0.002)
    val EURAUD = EURUSD.cross(AUDUSD)
    val roundedEURAUD: Rate = EURAUD.round(4)
    assert(1.5705==roundedEURAUD.getBid.getResult)
    assert(1.5751==roundedEURAUD.getOffer.getResult)
  }

  val rounder4dp: (Double) => Double = MathUtils.roundAt(4)
  def performBespokeComputation = {
    val EURUSD_BID=1.2333
    val EURUSD_OFFER=1.2338
    val AUDUSD_MID=0.7843
    val AUDUSD_SPREAD=0.002
    val AUDUSD_BID=AUDUSD_MID-AUDUSD_SPREAD*0.5
    val AUDUSD_OFFER=AUDUSD_MID+AUDUSD_SPREAD*0.5
    val EURAUD_BID=EURUSD_BID/AUDUSD_OFFER
    val EURAUD_OFFER=EURUSD_OFFER/AUDUSD_BID
    val EURAUD_BID_ROUNDED=rounder4dp(EURAUD_BID)
    val EURAUD_OFFER_ROUNDED=rounder4dp(EURAUD_OFFER)
    assert(1.5705==EURAUD_BID_ROUNDED)
    assert(1.5751==EURAUD_OFFER_ROUNDED)
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
    // Well, it's _just_ 60x slower
    def average(xs: Seq[Long]) = xs.foldLeft(0L)(_+_) / xs.size
    println("Average without explanation: " + average(results.map(x => x._1)))
    println("Average with explanation: " + average(results.map(x => x._2)))
    println("Average bespoke: " + average(results.map(x => x._3)))
  }
}