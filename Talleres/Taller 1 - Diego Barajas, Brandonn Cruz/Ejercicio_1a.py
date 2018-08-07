#Taller punto 1
##metodo de horner
def horner(x, polinomio, redondeo):
    y = polinomio[0]
    cantiopera = 0
    for i in range(1, len(polinomio)):
      y = y*x + polinomio[i]
      cantiopera = cantiopera + 2
    return [round(y, redondeo), cantiopera]

##Evaluacion
def evaluacion(x, polinomio, redondeo):
    final = 0
    cantiopera = 0
    for i in range(0, len(polinomio)):
      final = final + polinomio[i]*(x**(i))
      cantiopera = cantiopera + (len(polinomio)-1) + 2
    return [round(final, redondeo), cantiopera]

##Derivada
def horner_derivada(x, polinomio, redondeo):
    y = polinomio[0]*(len(polinomio)-1)
    cantiopera = 2
    for i in range(1, len(polinomio)-1):
        y = (y*x) + (polinomio[i]*(len(polinomio)-1-i))
        cantiopera = cantiopera + 5
    return [round(y,redondeo), cantiopera]
