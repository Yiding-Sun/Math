import org.junit.Test
import math._


class FNum(val child: Int, val mother: Int, val symbol: Boolean = true) extends Operational[FNum] {
	
	
	private def simplify = {
		val nSymbol = (child * mother > 0) == symbol
		var nChild = abs(child)
		var nMother = abs(mother)
		var i = 2
		while (i < min(nChild, nMother)) {
			while (nChild % i == 0 && nMother % i == 0) {
				nChild /= i
				nMother /= i
			}
			i += 1
		}
		new FNum(nChild, nMother, nSymbol)
	}
	
	override def +(arg: FNum): FNum = {
		val sThis = toStandard
		val sArg = arg.toStandard
		FNum(sThis.child * sArg.mother + sThis.mother * sArg.child, mother * arg.mother)
	}
	
	override def -(arg: FNum): FNum = this + (-arg)
	
	override def *(arg: FNum): FNum = FNum(child * arg.child, mother * arg.mother, symbol == arg.symbol)
	
	override def /(arg: FNum): FNum = this * (~arg)
	
	override def unary_-(): FNum = new FNum(child, mother, !symbol)
	
	def unary_~(): FNum = new FNum(mother, child, symbol)
	
	private def toStandard = new FNum(if (symbol) child else -child, mother)
	
	override def toString: String = (if (!symbol) " - " else "") + "[ " + child + " / " + mother + " ]"
	
	override def equals(obj: scala.Any): Boolean = toString.equals(obj.toString)
}

object FNum {
	def apply(child: Int, mother: Int): FNum = new FNum(child, mother) simplify
	
	def apply(child: Int, mother: Int, symbol: Boolean): FNum = new FNum(child, mother, symbol) simplify
	
	def unapply(arg: FNum): (Int, Int, Boolean) = (arg.child, arg.mother, arg.symbol)
	
	implicit def int2FNum(arg: Int): FNum = FNum(arg, 1)
}

class FNumTest {
	
	@Test
	def test(): Unit = {
		import org.junit.Assert._
		assertEquals("simplify1", "[ 2 / 5 ]", FNum(20, 50).toString)
		assertEquals("simplify2", "[ 1 / 5 ]", FNum(25, 125).toString)
		assertEquals("simplifyN1", " - [ 1 / 5 ]", FNum(25, -125).toString)
		assertEquals("simplifyN2", " - [ 1 / 5 ]", FNum(-25, 125).toString)
		assertEquals("simplifyN3", "[ 1 / 5 ]", FNum(-25, 125, symbol = false).toString)
		assertEquals("plus1", FNum(1, 2), FNum(1, 6) + FNum(1, 3))
		assertEquals("plus2", -FNum(1, 2), -FNum(1, 6) + (-FNum(1, 3)))
		assertEquals("plus3", -FNum(1, 2), FNum(1, 6, symbol = false) + FNum(1, 3, symbol = false))
		assertEquals("minus1", FNum(1, 6), FNum(1, 2) - FNum(1, 3))
		assertEquals("minus2", FNum(1, 6), -FNum(1, 3) + FNum(1, 2))
		assertEquals("mult1", FNum(2, 3), FNum(1, 6) * FNum(4, 1))
		assertEquals("mult2", -FNum(2, 3), FNum(1, 6) * FNum(4, 1, symbol = false))
		assertEquals("divide1", FNum(2, 3), FNum(1, 6) / FNum(1, 4))
		assertEquals("divide2", -FNum(2, 3), FNum(1, 6) / (-FNum(1, 4)))
		assertEquals("implicit", FNum(1, 6), 1 / FNum(6, 1))
		assertEquals("implicit2", FNum(1, 6), FNum(1, 3) / 2)
	}
}
