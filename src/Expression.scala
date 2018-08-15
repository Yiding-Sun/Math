abstract class Expression extends Operational[Expression]
abstract class Operational[T]{
	def +(arg: T):T
	def -(arg: T):T
	def *(arg: T):T
	def /(arg: T):T
	def unary_-():T
}
