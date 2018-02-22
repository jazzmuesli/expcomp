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
}