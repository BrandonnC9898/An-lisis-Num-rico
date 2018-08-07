#Taller 1 Ejercicio_5
#Propagacion del error
import math

def error_relativo(valor, absoluto):
    return abs(round((absoluto/valor), 8))

def error_absoluto(valor_promedio, valor_maximo):
    return round(valor_maximo - valor_promedio, 8)

def distancia(velocidad, tiempo):
    return round(tiempo*velocidad, 8)
