def fac(n:int)->int:
	cumul = 1
	next = 2
	def fact_iter(cumul:int, next:int):
		if next == n:
			return cumul
		return fact_iter(cumul*next, next+1)
	return fact_iter(cumul, next)

print(fac(6))