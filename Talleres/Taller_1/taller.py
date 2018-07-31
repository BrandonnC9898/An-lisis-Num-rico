import math

def funcion(x):
	return round((((9.8*68.1)/x)*(1-math.exp(-(x/68.1)*10))-40), 8)

def secc(x, y, particiones):
	fin = []
	seccion = round(abs((y)-(x))/particiones, 8)
	fin.append(x)
	for i in range(0, particiones-1):
		fin.append(round(fin[i]+seccion, 8))
	fin.append(round(y, 8))
	#print("ESTE ES FIN:", fin)
	return fin


def n_seccionar(intervalo, particiones, cantidad_iteraciones):
	separaciones = secc(intervalo[0], intervalo[1], particiones)
	#print("SEPARA:", separaciones)
	#caso Base
	for i in range(0, len(separaciones)):
		if(abs(funcion(separaciones[i])) <= 0.00000001):
			return separaciones[i]

	valor = funcion(separaciones[0])
	for i in range(1, len(separaciones)):
		#print("--",i, separaciones[i])
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

def seccionar(intervalo, particiones, tolerancia):
	#Definicion de variables
	cantidad_iteraciones = 1
	separaciones = secc(intervalo[0], intervalo[1], particiones)
	Xr_actual = round((separaciones[0]+separaciones[len(separaciones)-1])/2, 8)
	error_comparacion = -1
	#do_while emulado
	conta = 1
	while True:
		cantidad_iteraciones = cantidad_iteraciones + 1
		Xr_anterior = Xr_actual
		f_Xr = round(funcion(Xr_anterior),8)

		valor = round(((funcion(separaciones[0])) > 0), 8)
		for i in range(1, len(separaciones)):
			valorAux = round(((funcion(separaciones[i])) > 0), 8)
			if(valor == valorAux):
				valor = valorAux
			else:
				separaciones = secc(separaciones[i-1], separaciones[i], particiones)
				break

		Xr_actual = round((separaciones[0]+separaciones[len(separaciones)-1])/2, 8)
		error_p = abs(round(((Xr_actual - Xr_anterior)/Xr_actual)*100, 8))
		if(error_p/error_comparacion > 0):
			print("K=", error_p/error_comparacion)
		error_comparacion = error_p
		if(error_p <= tolerancia):
			return [Xr_actual, cantidad_iteraciones, error_p]



intervalo = [12, 16]
tolerancia = 1 #1%
print("Intervalo de 12 a 16")
#3 secciones
cantidad_secciones = 2
resultados = seccionar(intervalo, cantidad_secciones, tolerancia) #La tolerancia se mide en porcentajes
print(cantidad_secciones, "secciones")
print("\tXr:", resultados[0])
print("\tcantidad_iteraciones:", resultados[1])
print("\tError porcentual:", resultados[2], "%")

#4 secciones
cantidad_secciones = 4
resultados = seccionar(intervalo, cantidad_secciones, tolerancia) #La tolerancia se mide en porcentajes
print(cantidad_secciones, "secciones")
print("\tXr:", resultados[0])
print("\tcantidad_iteraciones:", resultados[1])
print("\tError porcentual:", resultados[2], "%")

print("\n\nValor Exacto:", n_seccionar(intervalo, cantidad_secciones, [0]))
