import math

def funcion(x,y, redondeo):
	return round(x ** y, redondeo)

def secc(x, y, particiones, redondeo):
	fin = []
	seccion = round(abs((y)-(x))/particiones, redondeo)
	fin.append(x)
	for i in range(0, particiones-1):
		fin.append(round(fin[i]+seccion, redondeo))
	fin.append(round(y, redondeo))
	#print("ESTE ES FIN:", fin)
	return fin


def raiz_binario(radicando, indice, redondeo, x_i, x_d, valor_aux):
	valor = round((x_d+x_i)/2, redondeo)
	resultado = funcion(valor , indice, redondeo)
	if( radicando == resultado or valor_aux == valor):
		return valor
	else:
		valor_aux = valor
		if(resultado > radicando):
			return raiz_binario(radicando, indice, redondeo, x_i, round((x_d+x_i)/2, redondeo), valor)
		if(resultado < radicando):
			return raiz_binario(radicando, indice, redondeo, round((x_d+x_i)/2, redondeo), x_d, valor)
	return valor
