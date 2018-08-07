#Metodo de newton raphson

from Ejercicio_1a import *


#def funcion(x, redondeo):
#    return round(((8*math.sin(x) - 3*math.cos(2x) - 2cos(x))/2*math.sqrt(4*(math.cos(2)**2) + (sin(x)**2) - 8*(cos(x)) - 2*sin(x) + 5)), redondeo)


def newton(iteraciones, aproximado, redondeo):
    x = aproximado
    canti_itera = 0
    for i in range(1, iteraciones+1):
        s = horner(x, funcion, 8)
        sd = horner_derivada(x, funcion, 8)
        x_aux = x - (s[0]/sd[0])
        canti_itera = canti_itera + 1
        if(x_aux == x):
            break
        x = x_aux
    return [round(x ,redondeo), canti_itera]

#Main ejercicio_7
funcion = [1, 0, -1, -1]
redondeo = 4
metodo = newton(100, 5, 4)
print("raiz de la funcion:", metodo[0], "cantidad_iteraciones:", metodo[1])
