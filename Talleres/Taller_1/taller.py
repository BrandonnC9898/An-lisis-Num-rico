import math
import timeit

def funcion(x):
	return round((((9.8*68.1)/x)*(1-math.exp(-(x/68.1)*10))-40), 8)


def secc(intervalo, particiones):
	fin = []
	seccion = round(abs((intervalo[1])-(intervalo[0]))/particiones, 8)
	fin.append(intervalo[0])
	for i in range(0, particiones-1):
		fin.append(round(fin[i]+seccion, 8))
	fin.append(round(intervalo[1], 8))
	return fin

def n_seccionar(intervalo, particiones, cantidad_iteraciones):
	separaciones = secc(intervalo, particiones)
	for i in range(0, len(separaciones)):
		if(abs(funcion(separaciones[i])) <= 0.00000001):
			return separaciones[i]

	valor = funcion(separaciones[0])
	for i in range(1, len(separaciones)):
		valorAux = funcion(separaciones[i])
		valorAux = round(valorAux, 8)
		if((valor < 0 and valorAux < 0) or (valor > 0 and valorAux > 0)):
			valor = valorAux
		else:
			nuevoIntervalo = []
			nuevoIntervalo.append(separaciones[i-1])
			nuevoIntervalo.append(separaciones[i])
			cantidad_iteraciones[0] = cantidad_iteraciones[0] + 1
			return n_seccionar(nuevoIntervalo, particiones, cantidad_iteraciones)



intervalo = [10, 20]
print("Intervalo de 10 a 20")
cantidad_secciones = 3
cantidad_iteraciones = [0]
print("Respuesta con 3 secciones:", n_seccionar(intervalo, cantidad_secciones, cantidad_iteraciones), "+- 0.00000001")
print("cantidad_iteraciones:", cantidad_iteraciones)
cantidad_secciones = 4
print("Respuesta con 4 secciones:", n_seccionar(intervalo, cantidad_secciones, cantidad_iteraciones), "+- 0.00000001")
print("cantidad_iteraciones:", cantidad_iteraciones)
