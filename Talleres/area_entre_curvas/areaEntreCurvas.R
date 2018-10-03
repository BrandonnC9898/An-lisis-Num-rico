seno = function(x){
  return(exp(x))
}

coseno = function(x){
  return(log(x, base=exp(1)))
}

expresion1 = function(x) x^2
expresion2 = function(x) x

centro = function(valor, precision, ini){
  final = 0
  inicio = (ini*precision)
  valor = (valor*precision)-1
  for (i in inicio:valor) {
    dato = i/precision
    if(seno(dato) > coseno(dato)){
      calculo = (seno(dato)*(1/precision))-(coseno(dato)*(1/precision))
    }else{
      calculo = (coseno(dato)*(1/precision))-(seno(dato)*(1/precision))
    }
    final = final + calculo
  }
  return (final)
}

inicio = 0
final = 4
precision = 1000
x = seq(inicio,final,by=1/precision)
y = seno(x)
y1 = coseno(x)

integral = centro(final,precision,inicio)
cat(integral)

xx = c(x,rev(x))
yy = c(y,rev(y1))

plot(x,y,type="l",asp=1,col="red")
lines(x,y1,col="green")
polygon(xx,yy,col = "skyblue")