#Taller 1 Ejercicio_4
#Truncamiento, error relativo y error absoluto
import math

def error_relativo(valor, capacidad): #capacidad=cantidad de decimales soportados por el dispositivo, valor=valor al cual se le calcula el error
    return abs(10 * (10**(-capacidad)))

def error_absoluto(valor, capacidad): #capacidad=cantidad de decimales soportados por el dispositivo, valor=valor al cual se le calcula el error
    n = int(math.log10(valor)) + 1
    return abs(1 * (10**(n-capacidad)))


capacidad = 4
valor = 534.78

print("Los errores para el numero", valor, "en un dispositivo que solo almacena" , capacidad, "decimales son")
print("Error absoluto <", error_absoluto(536.78, 4))
print("Error relativo <", error_relativo(536.78, 4))
