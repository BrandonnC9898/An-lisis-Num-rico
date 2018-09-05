##Analisis numerico interpolacion 2018


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

# --- Prueba
x=c(0.2,0.3,0.4,0.5)
y=c(0.32,0.33,0.34,0.45)
z0 = lagrange(x[1:4],y[1:4],0.2)
z1 = lagrange(x[1:4],y[1:4],0.3)
z2 = lagrange(x[1:4],y[1:4],0.4)
z3 = lagrange(x[1:4],y[1:4],0.5)

polyAjuste = poly.calc(x,y)

plot(x,c(z0,z1,z2,z3), pch=19, cex=1, col = "red", asp=1)
curve(polyAjuste,add=T)


x1=c(0.2,0.4,0.6)
y1=1:length(x1)
z4=1:length(x1)

for (i in 1:length(x1)) {
  y1[i]=1/(1+25*x1[i]^2)
}

for (i in 1:length(x1)) {
  z4[i]=lagrange(x1[1:length(x1)],y1[1:length(y1)],x1[i])
}

polyAjuste2 = poly.calc(x1,y1)

x=seq(-1,1,0.01)
plot(x, 1/(1+25*x^2),type="l",col="blue",lwd=3)
points(x1, z4, pch=19, cex=1, col = "red", asp=1,xlab="X", ylab="Y", main="interpolar")
curve(polyAjuste2,add=T)
