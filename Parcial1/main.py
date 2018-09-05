#parcial
from random import randrange
import math

def consistencia(tam, maximoValor):
    a = []
    b = []
    c = []
    d = []
    a.append(0)
    for i in range(1, tam):
        a.append(randrange(1, maximoValor+1))
        b.append(randrange(1, maximoValor+1))
        c.append(randrange(1, maximoValor+1))
        d.append(randrange(1, maximoValor+1))
    c.append(0)
    b.append(randrange(1, maximoValor+1))
    d.append(randrange(1, maximoValor+1))
    return [a,b,c,d]


def TDMASolve(a, b, c, d):
	nmax = len(d)#n in the numbers is rows

	# Modify the first-row coefficients
	c[0] /= b[0] #Division by zero risk.
	d[0] /= b[0]

	for i in range(1, nmax):
		ptemp = b[i] - (a[i] * c[i-1])
		c[i] /= ptemp
		d[i] = (d[i] - a[i] * d[i-1])/ptemp

	#Back Substitution
	x = [0 for i in range(nmax)]
	x[-1] = round(d[-1], 5)
	for i in range(-2,-nmax-1,-1):
		x[i] = round(d[i] - c[i] * x[i+1], 5)
	return x

def converge(x, a, b, c, d):
	for i in range(len(d)):
		r = (a[i] * x[i - 1]) + (b[i] * x[i]) + (c[i] * x[i - 2]) 
		e = abs(r - d[i])
		if (e <= 0.00001):
			return -1
	return 0

def copiar(a, b):
	for i in range(len(a)):
		b[i] = a[i]

def sumarElementos(a):
	suma = 0
	for i in range(len(a)):
		for j in range(len(a)):
			suma = suma + a[i][j]
	return suma

def funcion(x):
	return math.log(x + 2) - math.sin(x)

def recursivaF(x, e):
	x1 = funcion(x)
	return recursiva(x1, x, e)

def recursiva(x, x1, e):
	if (abs(x - x1) <= e):
		return x
	else:
		y = x - (funcion(x)*(x-x1))/(funcion(x)-funcion(x-1))
		print(y)
		recursiva(y, x, e)

#main
print("Punto 3.d")
a = [0,2,6,9]
b = [7,-8,4,8]
c = [5,1,3,0]
d = [6,5,7,8]
print("para a = ", a, "b = ", b, "c = ", c, "y d = ", d)
a1 = [0 for i in range(len(d))]
b1 = [0 for i in range(len(d))]
c1 = [0 for i in range(len(d))]
d1 = [0 for i in range(len(d))]
copiar(a, a1)
copiar(b, b1)
copiar(c, c1)
copiar(d, d1)
x = TDMASolve(a, b, c, d)
print("Solucion x = ", x)
print("converge? ", converge(x, a1, b1, c1, d1))
#1.b------------------------
print("Punto 1.b")
a = [[1,1,1],[1,1,1],[1,1,1]]
print("Para a = ", a)
print(sumarElementos(a))
a = [None]*100
for i in range(100):
	a[i] = [1 for j in range(100)]
print("Para a de tamaño 100 x 100}")
print(sumarElementos(a))
#2.a------------------------
print("Punto 2.a")
print(recursivaF(-1.5, 0.0000001))
print("End")