
import ComplexImplicits._

import scala.language.implicitConversions

/**
  * Global settings for NumericExpressions
  * You can disable explanation and keep just the result of computations
  */
object GlobalExplanationSettings {
  var explain = true
}
object ComplexImplicits {
  implicit def Double2Number(value : Double) = new NumericExpression(value)
  implicit def Int2Number(value : Int) = new NumericExpression(value)
}

class NumericExpression(result:Double,
                        leftOperand: Option[NumericExpression]=None,
                        operation: Option[String]=None,
                        rightOperand: Option[NumericExpression]=None) {
  override def toString = if (operation.isDefined && rightOperand.isDefined) "(" + result + "=" + leftOperand.get + " " + operation.get + " " + rightOperand.get + ")" else String.valueOf(result)
  def +(that: NumericExpression) = applyOperation(that, "+")
  def -(that: NumericExpression) = applyOperation(that, "-")
  def /(that: NumericExpression) = applyOperation(that, "/")
  def *(that: NumericExpression) = applyOperation(that, "*")
  def round(that: NumericExpression) = applyOperation(that, "round")
  def getResult: Double = result
  def getLeftOperand = leftOperand
  def getOperation = operation
  def getRightOperand = rightOperand
  private def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }

  private def applyOperation(that: NumericExpression, op: String) = {
    val result: Double = evaluate(getResult, op, that.getResult)
    if (GlobalExplanationSettings.explain) {
      new NumericExpression(result,Some(this), Some(op),Some(that))
    } else {
      Number(result)
    }
  }
  private def evaluate(a: Double, op: String, b: Double): Double = {
    op match {
      case "*" => a*b
      case "+" => a+b
      case "-" => a-b
      case "/" => a/b
      case "round" => roundAt(b.toInt)(a)
      case _ => throw new IllegalArgumentException("Unknown operation: " + op)
    }
  }
}

case class Number(n: Double) extends NumericExpression(n)

class Rate(left: NumericExpression, right: NumericExpression) {
  def calculateMid = (left+right)*0.5
  def getBid = left
  def getOffer = right
  def cross(that: Rate) = new Rate(left/that.getOffer,right/that.getBid)
  def spread = right - left
  def round(dp: Int) = new Rate(left.round(dp),right.round(dp))
  override def toString: String = "Rate(bid=" + left + ", offer=" + right + ")"
}
class SpreadedRate(mid: NumericExpression, spread: NumericExpression) extends Rate(mid-spread*0.5,mid+spread*0.5) {
  override def toString: String = super.toString + "=SpreadedRate(mid=" + mid + ", spread=" + spread + ")"
}


