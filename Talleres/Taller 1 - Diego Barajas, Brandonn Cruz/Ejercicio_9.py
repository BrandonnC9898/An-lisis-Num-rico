import math

def funcionG(x):
	return round(((1 + math.exp(x)) / 5), 8)


def puntoFijo(intervalo, x0, tolerancia):
	e = 1
	while(e >= tolerancia):
		x1 = funcionG(x0)
		e = abs(x1-x0)
		x0 = x1
		print(x1)
	return [x1, e]
