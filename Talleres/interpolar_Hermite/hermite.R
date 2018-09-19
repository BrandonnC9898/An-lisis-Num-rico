#Interpolación hermite

hermite = function(x,y){
  v = 1:(length(x)-1)
  for (i in 1:(length(x)-1)) {
    v[i] = (y[i+1]-y[i])/(x[i+1]-x[i])
  }
  M = cbind(v)
  for(i in 2:(length(x)-1)){
    v1 = herHelper(x,v,i)
    M = cbind(M,v1)
    v = v1
  }
  return(M)
}

herHelper = function(x,z,paso){
  v = rep(0, times=length(x)-1)
  ii = 1
  for(i in paso:length(z)){
    v[i] = (z[i]-z[i-1])/(x[ii+paso]-x[ii])
    ii = ii+1
  }
  return(v)
}

obtenertAn = function(M,y){
  A = 1:length(y)
  A[1] = y[1]
  for (i in 1:(length(y)-1)) {
    A[i+1] = M[i,i]
  }
  return(A)
}

calcularPolinomio = function(A,x,z){
  r = A[1]
  p = 1
  for (i in 2:length(A)) {
    p = (z-x[i-1])*p
    r = r+(A[i]*p)
  }
  return(r)
}

graficarHermite = function(A,x,y){
  z = seq(x[1],x[length(x)],length=100 )
  z1 = rep(0,times=length(z))
  for (i in 1:length(z)) {
    z1[i] = calcularPolinomio(A,x,z[i])
  }
  plot(z,z1,type="l",asp=1)
  points(x, y, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="interpolar")
}

invertirV = function(v,m){
  for (i in 1:length(v)) {
    v[i] = m-v[i]
  }
  return(v)
}

limitarV = function(v,i,f){
  return(v[i:f])
}

generarZ = function(x,cant){
  z = seq(x[1],x[length(x)],length=cant )
  return(z)
}

generarZ1 = function(A,x,z){
  z1 = rep(0,times=length(z))
  for (i in 1:length(z)) {
    z1[i] = calcularPolinomio(A,x,z[i])
  }
  return(z1)
}

calcularZs = function(cordsx,cordsy,cant,tam){
  zf = c()
  zff = c()
  for (i in 1:tam) {
    M = hermite(cordsx[i,],cordsy[i,])
    A = obtenertAn(M,cordsy[i,])
    z = generarZ(cordsx[i,],100)
    zi = generarZ1(A,cordsx[i,],z)
    zf = c(zf,z)
    zff = c(zff,zi)
  }
  zetas = rbind(zf,zff)
  return(zetas)
}

x = c(14.5, 20.5, 26.5, 32)
y = c(27.5, 29, 28, 26)

x1 = c(32,37,47,53.5)
y1 = c(26,27.5,27.5,27)

x2 = c(53.5,57.5,63,68.5)
y2 = c(27,31.5,36.5,39.5)

x3 = c(68.5,68,64,61) #x3 = c(68.5,68,66.5,64,59)
y3 = c(39.5,53,89,99) #y3 = c(39.5,53,69.5,89,106)

x4 = c(59,79,92.5,102.5) #x4 = c(59,79,92.5,100.5,102.5)
y4 = c(106,94.5,69.5,46.5) #y4 = c(106,94.5,69.5,48,46.5)

x5 = c(102.5,109.5,118.5,125.5)
y5 = c(46.5,47,45.5,41)

x6 = c(125.5,132,139.5,146)
y6 = c(41,41.5,41,39)

x7 = c(146,148,151.5,155)
y7 = c(39,39,39,38)

x8 = c(155,146.5,139,135.5)
y8 = c(38,35,34.5,32.5)

x9 = c(135.5,103,79,57.5)
y9 = c(32.5,15.5,13.5,16.5)

x10 = c(57.5,50.5,43,27) #x10 = c(57.5,50.5,43,34.5,29.5,27)
y10 = c(16.5,13,9,19) #y10 = c(16.5,13,9,10,14,19)

x11 = c(27,23,18.5,14.5)
y11 = c(19,22,24,27.5)

y = invertirV(y,50)
y1 = invertirV(y1,50)
y2 = invertirV(y2,50)
y3 = invertirV(y3,50)
y4 = invertirV(y4,50)
y5 = invertirV(y5,50)
y6 = invertirV(y6,50)
y7 = invertirV(y7,50)
y8 = invertirV(y8,50)
y9 = invertirV(y9,50)
y10 = invertirV(y10,50)
y11 = invertirV(y11,50)

cordsx = rbind(x,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
cordsy = rbind(y,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)

zetas = calcularZs(cordsx,cordsy,100,12)

xf = c(x,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11)
yf = c(y,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)

plot(zetas[1,],zetas[2,],type="l",asp=1)
points(xf, yf, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="interpolar")

