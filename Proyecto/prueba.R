# Este script se creo con el propósito de mostar algunos ejemplos del uso de la librería Richardson.
# Por: Brandonn Cruz y Diego Barajas.

library(Richardson)

imprimir = function(datos,derivada){
  cat("x\t\ty\t\tR\t\te\tanalitica\n")
  for(i in 1:length(datos$x)){
    cat(datos$x[i],"\t",datos$y[i],"\t",datos$resultados[i],"\t",datos$error[i],"\t",derivada[i],"\n")
  }
}

fun = function(x){return(x^2)}
derFun = function(x){return(2*x)}

x = seq(-1,3,by=0.25)
y = fun(x)

datos = derivada(x,y)
d = derFun(datos$x)
imprimir(datos,d)

fun2 = function(x){return(x*exp(x))}
derFun2 = function(x){return(exp(x)+exp(x)*x)}

y2 = fun2(x)

datos = derivada(x,y2,h=0.1,n=2)
d = derFun2(datos$x)
imprimir(datos,d)

fun3 = function(x){return(sin(x)+cos(x))}
derFun3 = function(x){return(cos(x)-sin(x))}

x = seq((-2*pi),(2*pi),by=(pi/8))
y3 = fun3(x)

datos = derivada(x,y3,h=0.001,n=8)
d = derFun3(datos$x)
imprimir(datos,d)

