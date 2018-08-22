from random import randrange

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
	x[-1] = d[-1]
	for i in range(-2,-nmax-1,-1):
		x[i] = d[i] - c[i] * x[i+1]
	return x

def converge(x, a, b, c, d):
	for i in range(len(d)):
		if ( (a[i] * x[i - 1]) + (b[i] * x[i]) + (c[i] * x[i - 2]) != d[i]):
			return -1
	return 0

def copiar(a, b):
	for i in range(len(a)):
		b[i] = a[i]

#main

tam = 5
tamMaximo = 6
paso = 1
valorMaximo = 10
print("Prueba de consistencia")
for i in range(tam, tamMaximo, paso):
    print("prueba de matriz tamanio", i)
    vectores = consistencia(i, valorMaximo)
    a = vectores[0]
    b = vectores[1]
    c = vectores[2]
    d = vectores[3]
    print("a", a)
    print("b", b)
    print("c", c)
    print("d", d)
    a1 = [0 for i in range(len(a))]
    b1 = [0 for i in range(len(d))]
    c1 = [0 for i in range(len(d))]
    d1 = [0 for i in range(len(d))]
    copiar(a, a1)
    copiar(b, b1)
    copiar(c, c1)
    copiar(d, d1)
    print("Solucion")
    x = TDMASolve(a, b, c, d)
    print("Convergencia")
    print(x)
    print(converge(x, a1, b1, c1, d1))
	
#Ejemplo de convergencia
a = [0,-4,5]
b = [2,-5,2]
c = [3,4,0]
d = [1,-11,9]
a1 = [0 for i in range(len(d))]
b1 = [0 for i in range(len(d))]
c1 = [0 for i in range(len(d))]
d1 = [0 for i in range(len(d))]
copiar(a, a1)
copiar(b, b1)
copiar(c, c1)
copiar(d, d1)
x = TDMASolve(a, b, c, d)
print("Ejemplo 2:")
print("para a = ", a, "b = ", b, "c = ", c, "y d = ", d)
print("Solucion x = ", x)
print("converge? ", converge(x, a1, b1, c1, d1))
