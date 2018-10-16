#Script by A. Roberts, 2013.
#To run the script, copy and paste it onto the R command line, and press ,<enter>.

KochSnowflakeExample <- function(){
  iterate <- function(T,i){
    A = T[ ,1]; B=T[ ,2]; C = T[,3];
    if (i == 1){
      d = (A + B)/2; h = (C-d); d = d-(1/3)*h;
      e = (2/3)*B + (1/3)*A; f = (1/3)*B + (2/3)*A;
    }
    
    if (i == 2){
      d = B; e = (2/3)*B + (1/3)*C; f = (2/3)*B + (1/3)*A;
    }
    
    if (i == 3){
      d = (B + C)/2; h = (A-d); d = d-(1/3)*h;
      e = (2/3)*C + (1/3)*B; f = (1/3)*C + (2/3)*B;
    }
    
    if (i == 4){
      d = C; e = (2/3)*C + (1/3)*A; f = (2/3)*C + (1/3)*B;
    }
    
    if (i == 5){
      d = (A + C)/2; h = (B-d); d = d-(1/3)*h;
      e = (2/3)*A + (1/3)*C; f = (1/3)*A + (2/3)*C;
    }
    
    if (i == 6){
      d = A; e = (2/3)*A + (1/3)*C; f = (2/3)*A + (1/3)*B;
    }
    
    if (i == 0){
      d = A; e = B; f = C;
    }
    
    Tnew = cbind(d,e,f)
    return(Tnew); #Return a smaller triangle.
  }
  
  draw <- function(T, col=rgb(0,0,0),border=rgb(0,0,0)){
    polygon(T[1,],T[2,],col=col,border=border)
  }
  
  Iterate = function(T,v,col=rgb(0,0,1),border=rgb(0,0,1)){
    for (i in v) T = iterate(T,i);
    draw(T,col=col,border=border);
  }
  
  recursiones = function(T0,n,array){
    if(n == 1){
      for(i in 0:6){
        Iterate(T0,c(array,i))
      }
    }else{
      aux = array
      for(i in 0:6){
        array = c(aux,i)
        recursiones(T0,(n-1),array)
      }
    }
  }
  
  distancia = function(x0,x1,y0,y1){
    return(sqrt((x1-x0)^2+(y1-y0)^2))
  }
  
  #The vertices of the initial triangle:
  A = matrix(c(1,0),2,1);
  B = matrix(c(cos(2*pi/3), sin(2*pi/3)),2,1);
  C = matrix(c(cos(2*pi/3),-sin(2*pi/3)),2,1);
  T0 = cbind(A,B,C);
  l0 = distancia(B[1,1],A[1,1],B[2,1],A[2,1])*3
  
  plot(numeric(0),xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),axes=TRUE,frame=TRUE,ann=FALSE);
  par(mar=c(0,0,0,0),bg=rgb(1,1,1));
  par(usr=c(-1.1,1.1,-1.1,1.1));
  
  #Draw snowflake:
  #recursiones: Al ser ineficiente el código, más de 6 iteraciones tarda demasiado.
  n = 5
  recursiones(T0,n,c())
  longitud = ((4/3)^n)*l0
  cat("longitud de curva = ",longitud)
}
KochSnowflakeExample() #Run the example.
