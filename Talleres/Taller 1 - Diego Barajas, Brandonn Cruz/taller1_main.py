from Ejercicio_1a import *
from Ejercicio_5 import *
from Ejercicio_13 import *
from Ejercicio_2 import *
from Ejercicio_6 import *
from Ejercicio_9 import *
#----------------------------------------------------------------------------------------------------
#EJERCICIO_1a
print("\nEJERCICIO 1-a\n")
redondeo = 8
x = -2
#horner
polinomio = [2, 0, -3, 3, -4]
resultado = horner(x, polinomio, redondeo)
derivada_horner = horner_derivada(x, polinomio, redondeo)
print("Evaluacion con horner al polinomio,", redondeo, "cifras significativas")
print("\tResultado:", resultado[0])
print("\tcantidad operaciones:", resultado[1])
print("\nEvaluacion con horner a la derivada,", redondeo, "cifras significativas")
print("\tResultado", derivada_horner[0])
print("\tcantidad operaciones:", derivada_horner[1])


#----------------------------------------------------------------------------------------------------
#EJERCICIO_2
print("\nEJERCICIO 2\n")
intervalo = [0, 30]
tolerancia = 0.0001 #1%
print("Intervalo de", intervalo[0], "a", intervalo[1])
x = 32
y = 24
v = 10
#3 secciones
cantidad_secciones = 2
resultados = seccionar(intervalo, cantidad_secciones, tolerancia, x, y, v) #La tolerancia se mide en porcentajes
print("Metodo 1 biseccion")
print("\tXr:", resultados[0])
print("\tcantidad_iteraciones:", resultados[1])
print("\tError porcentual:", resultados[2], "%")
print("\tResultado de la funcion:", funcion(resultados[0],x,y,v))
print("\tVolumen calculado:", round((x-2*resultados[0])*(y-2*resultados[0])*resultados[0], 8))
cantidad_secciones = 3
resultados = seccionar(intervalo, cantidad_secciones, tolerancia, x, y, v) #La tolerancia se mide en porcentajes
print("Metodo 2 triseccion")
print("\tXr:", resultados[0])
print("\tcantidad_iteraciones:", resultados[1])
print("\tError porcentual:", resultados[2], "%")
print("\tResultado de la funcion:", funcion(resultados[0],x,y,v))
print("\tVolumen calculado:", round((x-2*resultados[0])*(y-2*resultados[0])*resultados[0], 8))

#--------------------------------------------------------------------------------------------------
print("\nEJERCICIO 5\n")
#Main Ejercicio_5
velocidad = 4 #metros/segundos
error_velocidad = 0.1
tiempo = 5 #segundos
error_tiempo = 0.1
redondeo = 8

distancia_maxima = distancia(velocidad + error_velocidad, tiempo + error_tiempo)
distancia_minima = distancia(velocidad - error_velocidad, tiempo - error_tiempo)
distancia_promedio = round((distancia_maxima + distancia_minima) / 2, redondeo)
print("Para una velocidad de ", velocidad, "+-", error_velocidad, "m/s y un tiempo de", tiempo, "+-:", error_tiempo, "s")
print("\tDistancia promedio:", distancia_promedio, "m")
print("\tError absoluto:", error_absoluto(distancia_promedio, distancia_maxima))
print("\tError relativo:", error_relativo(velocidad*tiempo, error_absoluto(distancia_promedio, distancia_maxima))*100, "%")

#----------------------------------------------------------------------------------------------------
#EJERCICIO_6
print("\nEJERCICIO 6\n")
print("\nn =", funcionD(73))

#--------------------------------------------------------------------------------------------------
print("\nEJERCICIO 13\n")
#Main Ejercicio_13
#Valores iniciales
x_i = -20
x_d = 20
redondeo = 8

indice = 2
radicando = 7

print("Para el intervalo de ", x_i, "a", x_d, "con ",redondeo, "sifras significativas")
print("\n\tLa raiz con indice", indice, "y radicando", radicando, "es:")
print("\t", raiz_binario(radicando, indice, redondeo, x_i, x_d, -math.inf))

indice = 3
radicando = 32

print("Para el intervalo de ", x_i, "a", x_d, "con ",redondeo, "sifras significativas")
print("\n\tLa raiz con indice", indice, "y radicando", radicando, "es:")
print("\t", raiz_binario(radicando, indice, redondeo, x_i, x_d, -math.inf))


#----------------------------------------------------------------------------------------------------
#EJERCICIO_9
print("\nEJERCICIO 15\n")
intervalo = [-1, 5]
#print(puntoFijo(intervalo, 1.5, 0.0001))
r  = puntoFijo(intervalo, 1.5, 0.0001)
print(r[0]*1000000)
