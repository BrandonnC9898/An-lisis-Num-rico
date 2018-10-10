koch2 = function(x,y,n){
  longitud = 0
  
  r1 = triangulo(x[2:3],y[2:3])
  x1 = r1[1,]
  y1 = sacarVector(r1[2,])
  longitud = 2*r1[3,1]+longitud-y1[1]
  
  r2 = triangulo(y1[2:3],y[1:2])
  x2 = r2[1,]
  y2 = sacarVector(r2[2,])
  longitud = (2*r2[3,1])*2+longitud-y2[1]
  
  r3 = triangulo(y2[2:3],y[1:2])
  x3 = r3[1,]
  y3 = sacarVector(r3[2,])
  longitud = (2*r3[3,1])*4+longitud
  
  return(longitud)
}

koch = function(x,y,n,longitud){
  if(n <= 1){
    r = triangulo(x[2:3],y[2:3])
    longitud = (2*r[3,1])+(2*y[1])+longitud
  }else{
    r = triangulo(x[2:3],y[2:3])
    x1 = r[1,]
    y1 = sacarVector(r[2,])
    longitud = koch(y1,y,(n-1),longitud)+longitud
  }
  return(longitud)
}

triangulo = function(x,y){
  ca = (x[length(x)]-x[1])/2
  co = tan(pi/3)*ca
  x1 = 1:(length(x)+1)
  y1 = 1:(length(x)+1)
  mitad = length(x1)%%2+length(x1)%/%2
  contador = 1
  for(i in 1:length(x1)){
    if(i == mitad){
      x1[i] = ca+x[i-1]
      y1[i] = co+y[i-1]
    }else{
      x1[i] = x[contador]
      y1[i] = y[contador]
      contador = contador+1
    }
  }
  print(x1)
  print(y1)
  h = sqrt((co)^2+(ca)^2)
  hip = rep(h,length(x1))
  contenedor = rbind(x1,y1)
  contenedor = rbind(contenedor,hip)
  # plot(contenedor[1,],contenedor[2,],type="o",asp=1)
  return(contenedor)
}

unirPuntos = function(x,x1,y,y1){
  xf = 1:(length(x)+1)
  yf = 1:(length(x)+1)
  mitad = length(xf)%%2+length(xf)%/%2
  contador = 1
  for (i in 1:length(xf)) {
    if(i == mitad){
      xf[i] = x1[i-1]
      yf[i] = y1[i-1]
    }else{
      xf[i] = x[contador]
      yf[i] = y[contador]
      contador = contador+1
    }
  }
  contenedor = rbind(xf,yf)
  return(contenedor)
}

sacarVector = function(x){
  cant = (x[2]-x[1])/4
  xf = 1:4
  aux = x[1]
  for(i in 1:length(xf)){
    xf[i] = aux+cant*i 
  } 
  return(xf)
}

x = 1:4
y = rep(1,length(x))
longitud = koch(x,y,3,0)
print(longitud)
plot(x,y,type="l",asp=1)