import math

def horner(x, polinomio, redondeo):
    #Caso Base
    if(len(polinomio) == 0):
        return 0
    #Caso Recurrente
    else:
        valor = polinomio.pop()
        return round(valor + horner(x, polinomio, redondeo)*x, redondeo)


def raiz(polinomio, rango, paso, redondeo):
    fin = []
    inicio = rango[0]
    while(inicio != rango[1]):
        #print(inicio)
        datos = polinomio[:]
        inicio = round(inicio + paso, redondeo)
        evaluacion = horner(inicio, datos, redondeo)
        #print("Evaluacion", evaluacion)
        if(evaluacion == 0):
            fin.append(inicio)
    return fin


#Datos Base
polinomio = [4,4,4,4,4,4,4,4,1] #Polinomio de tal forma que L[0] = 4*x^0, L[1] = 4*x^1, ..., L[n] = 4*x^n
rango = [-1, 1] #Rango de valores en los que se buscan las raices
redondeo = 4 #valor de redondeo
paso = 1*(10**-redondeo)
print(raiz(polinomio, rango, paso, redondeo))
