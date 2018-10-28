
install.packages("pracma")
library(pracma)


metodoEuler <- function(f, h, xi, yi, xf){
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    y[i+1] = y[i]+(h*f(x[i],y[i]))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}

varEuler = function(f, h, xi, yi, xf){
  N = (xf - xi) / h
  x = y = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    yprima = y[i]+(h*f(x[i],y[i]))
    y[i+1] = y[i]+(h/2)*(f(x[i],y[i])+f(x[i+1],yprima))
    i = i+1
  }
  return (data.frame(X = x, Y = y))
}


TaylorO2 = function(f, h, xi, yi, xf){
  N = (xf - xi) / h
  x = y = e = numeric(N+1)
  x[1] = xi; 
  y[1] = yi;
  e[1] = 0
  i = 1
  while (i <= N)
  {
    x[i+1] = x[i]+h
    w = y[i]+h*f(x[i],y[i])
    y[i+1] = y[i]+(h*f(x[i],y[i]))+((h^2/2)*w)
    e[i+1] = abs(y[i+1]-y[i])
    i = i+1
  }
  return (data.frame(X = x, Y = y, E = e))
}



#TALLER
###PUNTO 1
par(mfrow=c(1,1))
h = 0.5; xi = 0; yi = 180; xf = 200;
E = 5.6*10^(-8); CY = 1; S = 6;
Te = 200; m = 1; c = 100; 

f <- function(x, y) {(-E*CY*S*(y^4-Te^4))/(m*c)}

e1 = metodoEuler(f,h,xi,yi,xf)
e1

xx <- c(0, 200); yy <- c(0, 200)
vectorfield(f, xx, yy, scale = 1)
for (xs in seq(0, 190, by = 10)) 
{
  sol <- pracma::rk4(f, 0, 200, xs, 400)
  lines(sol$x, sol$y, col="purple", asp=1)
}

###PUNTO 2
par(mfrow=c(1,1))
f <- function(x, y) {1-x^2+(x+y)}
respuesta = function(x){return(exp(x)*(x^2*exp(-x)+x*exp(-x)+1))}
x = seq(0,0.5,by=0.1)
t1 = TaylorO2(f, 0.1, 0, 1, 0.5)
t1
plot(t1$X,t1$Y,type="l",asp=1,xlab="x",ylab="y",col="blue")
lines(x,respuesta(x),col="green")
legend("bottomleft",col=c("blue","green"),legend =c("Taylor","Exacta"), lwd=3, bty = "n")


###PUNTO 3

funcionSolucion <- function(x){
  return (exp(x)*(x^2*exp(-x)+x*exp(-x)+1))#((exp(x))*(((x^2)*(exp^(-1*x)))+((x)*(exp^(-1*x)))+(1)))
}


f <- function(x, y) {1-(x^2)+x+y}
e1 = metodoEuler(f, 0.1, 0, 1, 10)
par(mfrow=c(1,2))
xx <- c(-3, 3); yy <- c(-1, 1)
vectorfield(f, xx, yy, scale = 0.01)
for (xs in seq(-1, 1, by = 0.25)){
  sol <- pracma::rk4(f, -1, 1, xs, 100)
  lines(sol$x, sol$y, col="purple")
}


x <- seq(1, 20, by=1)
y <- seq(1, 20, by=1)
cat("\tx\ty\n")
for (i in 1:20){
  cat("x = ", e1[i, 1], "   \tEuler = ", e1[i, 2], "   \tSolucion Exacta = ",funcionSolucion(e1[i, 1]), "   \tError Truncamiento = ",funcionSolucion(e1[i, 1])-e1[i, 2],"\n")
  #cat("", e1[i, 1], "|||", e1[i, 2], "|||",funcionSolucion(e1[i, 1]), "|||",funcionSolucion(e1[i, 1])-e1[i, 2],"||--")
  x[i] = e1[i, 1]
  y[i] = e1[i, 2]
}
plot(x, y, type="l",asp=1,col="purple", main = "Punto 3")





###PUNTO 4 

funcionSolucion <- function(x){
  return (exp(x)*(x^2*exp(-x)+x*exp(-x)+1))#((exp(x))*(((x^2)*(exp^(-1*x)))+((x)*(exp^(-1*x)))+(1)))
}


f <- function(x, y) {1-(x^2)+x+y}
e1 = metodoEuler(f, 0.1, 0, 1, 10)
par(mfrow=c(1,2))
xx <- c(-3, 3); yy <- c(-1, 1)
vectorfield(f, xx, yy, scale = 0.01)
for (xs in seq(-1, 1, by = 0.25)){
  sol <- pracma::rk4(f, -1, 1, xs, 100)
  lines(sol$x, sol$y, col="purple")
}


f <- function(x, y) {1-(x^2)+x+y}
m <- 20
h <- 0.1
#defino el primer punto en X y Y, ademas asigno memoria a las listas
x <- seq(0, 0+m, by = 1)
y <- seq(1, 1+m, by = 1)

for (i in 1:m){
  k1 = h*f(x[i], y[i])
  k2 = h*f(x[i] + h, y[i]+k1)
  y[i+1] = y[i]+((1/2)*(k1+k2))
  x[i+1] = x[i] + h
}

for (i in 1:m){
  cat("\tx = ", x[i], "\ty = ", y[i], "\n")
}

par(mfrow=c(1,1))
plot(x, y, type="l",asp=1,col="purple",main= "Punto 4")
for (i in 1:20){
  cat("x = ", x[i], "   \tEuler = ", y[i], "   \tSolucion Exacta = ",funcionSolucion(x[i]), "   \tError Truncamiento = ",funcionSolucion(x[i])-y[i],"\n")
  #cat("||||", x[i], "||||", y[i], "||||",funcionSolucion(x[i]), "||||",funcionSolucion(x[i])-y[i],"\n")
}


###PUNTO 5

x = seq(0,1,by=0.1)

h = 0.1; xi = 0; yi = 1; xf = 0.9;

f <- function(x, y) {x+y+1-x^2}

e1 = metodoEuler(f,h,xi,yi,xf)
e1[,]

e2 = varEuler(f,h,xi,yi,xf)
e2[,]


###PUNTO 7 (Tener encuenta recargar la libreria PhaseR para evitar errores de sobreescritura de funciones con pracma)


list.of.packages <- c("phaseR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(phaseR)

f<-function(fcn,x,y){
  return(eval(fcn))
}

# Solo para prueba con dy=x+y, y(0)=1
obtenerErrorAbsoluto<-function(x,y){
  solucion=exp(x)*((-x*exp(-x))-exp(-x)+2)
  return(abs(y-solucion))
}

graficarCampoPendiente<-function(x0, xn, y0, yn, fcn, numpendientes, metodo){
  apma1 <- function(t, y, parameters){
    a <- parameters[1] 
    dy <- a*(f(fcn, t, y))
    list(dy)
  } 
  apma1.flowField <- flowField(apma1, x = c(x0, xn), 
                               y   = c(y0, yn), parameters = c(1), 
                               points = numpendientes, system = "one.dim", 
                               add = FALSE, xlab = "x", ylab = "y", 
                               main = metodo)
  grid()
}

graficarSolucionNumerica<-function (x, y){
  points (x, y, pch=20, col="blue")
  for (i in 2:length(x)){
    segments(x[i-1], y[i-1], x[i], y[i], col="red")
  }
}

rk4<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1        |k2        |k3        |k4       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h/2, y[i-1]+k2*(0.5))
    k4=h*f(dy, t[i-1]+h, y[i-1]+k3)
    y<-c(y, y[i-1]+1/6*(k1+2*k2+2*k3+k4))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",k4," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK4")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

rk3<-function(dy, ti, tf, y0, h, graficar=TRUE, numpendientes=10){
  t<-seq(ti, tf, h)
  y<-c(y0)
  cat("x    |y         |k1         |k2        |k3       |error absoluto\n")
  for(i in 2:length(t)){
    k1=h*f(dy, t[i-1], y[i-1])
    k2=h*f(dy, t[i-1]+h/2, y[i-1]+k1*(0.5))
    k3=h*f(dy, t[i-1]+h, y[i-1]-k1+2*k2)
    y<-c(y, y[i-1]+1/6*(k1+4*k2+k3))
    cat(t[i-1]," | ", y[i-1]," | ",k1," | ",k2," | ",k3," | ",obtenerErrorAbsoluto(t[i-1],y[i-1]),"\n")
  }
  if (graficar){
    graficarCampoPendiente(min(t), max(t), min(y), max(y), dy, numpendientes, "RK3")
    graficarSolucionNumerica(t, y)
  }
  rta<-list(w=y, t=t)
}

r<-rk4(expression(x+y+1-x^2), 0, 2, 1, 0.1)
r2<-rk3(expression(x+y+1-x^2), 0, 2, 1, 0.1)



funcionSolucion <- function(x){
  return (exp(x)*(x^2*exp(-x)+x*exp(-x)+1))#((exp(x))*(((x^2)*(exp^(-1*x)))+((x)*(exp^(-1*x)))+(1)))
}


par(mfrow=c(1,3))

r2<- rk3(expression(x+y+1-x^2), 0, 0.9, 1, 0.1)
r<- rk4(expression(x+y+1-x^2), 0, 0.9, 1, 0.1)

f <- function(x, y) {1-(x^2)+x+y}
e1 = metodoEuler(f, 0.1, 0, 1, 0.9)
xx <- c(-3, 3); yy <- c(-1, 1)
vectorfield(f, xx, yy, scale = 0.01)
for (xs in seq(-1, 1, by = 0.25)){
  sol <- pracma::rk4(f, -1, 1, xs, 100)
  lines(sol$x, sol$y, col="purple")
}


print("comparacion de resulados Euler, Rk3 ,Rk4 y exacta(obtenida al solucionar la ecuacion)")
for(i in 1:10){
  k3 = r2[[1]]
  k4 = r[[1]] 
  cat("x =  ", e1[i,1], "\tEuler = ", e1[i,2],"     \tRk3 = ", k3[i], "       \tRk4 = ", k4[i],"       \tExacta = ",funcionSolucion(e1[i,1]), "\n")
  #cat(e1[i,1], "|||", e1[i,2],"|||", k3[i], "|||", k4[i],"|||",funcionSolucion(e1[i,1]), "||--")
}



