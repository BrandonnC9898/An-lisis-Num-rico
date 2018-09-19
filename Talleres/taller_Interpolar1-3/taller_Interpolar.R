install.packages("Matrix")#instalar paquete
library(Matrix)
install.packages("PolynomF")#instalar paquete
library(PolynomF)

require(PolynomF)
lagrange = function(x,y,a){
  n = length(x)
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

#Nodos: (xi,e^x)
x = seq(0, 1, by =0.1) #xi de los nodos
y = 1:length(x) #yi de los nodos
z = 1:length(x) #resultados de lagrange

for (i in 1:length(y)) {
  y[i] = exp(x[i])
}

for (i in 1:length(x)) {
  z[i] = lagrange(x[1:length(x)],y[1:length(y)],x[i])
}

polyAjuste = poly.calc(x,y) #función que calcula el polinomio interpolante

plot(x,z, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="Interpolar") #grafica los puntos dados por el método de lagrange
curve(polyAjuste,add=T) #grafica el polinomio interpolante

cat("Forma de lagrange ", z)
