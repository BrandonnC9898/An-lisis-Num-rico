funcionX = function(x){
  return(sin(x))
}

longitudCurva = function(a,b,n){
  suma = 0
  datos = seq(a,b,by=((b-a)/n)) 
  i = 1
  for(i in 1:(length(datos)-1)){
    deltaY = funcionX(datos[i+1])-funcionX(datos[i])
    deltaX = datos[i+1]-datos[i]
    calculo = sqrt(1+(deltaY/deltaX)^2)*deltaX
    suma = calculo + suma
  }
  return(c(suma,datos))
}

graficar = function(datos,a,b,paso){
  x = seq(a,b,by=((b-a)/n))
  y = funcionX(x)
  plot(x,y,type="l",asp=1)
  lines(datos,funcionX(datos),col="green")
}

a = -pi
b = pi
n = 1000
r = longitudCurva(a,b,n)
cat(r[1])
datos = r[2:length(r)]
graficar(datos,-2*pi,2*pi,n)
datos
