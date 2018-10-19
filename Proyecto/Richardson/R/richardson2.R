integral <- function(x, y){

  pivotear <- function(ini, fin){
    val = ini + fin
    if((val %% 2) == 0){
      pivote = (val/2)
    }
    else{
      pivote = ((val+1)/2)
    }
    return(pivote)
  }

  trapecio <- function(x1, y1, x2, y2){
    h = x2-x1
    area = (h/2)*((y1+y2))
    return(area)
  }

  if(length(x) != length(y)){
    stop("Los valores para 'Y' no coinciden con los valores en 'X'")
  } else if(length(x) < 3){
    stop("Se nesesitan mas valores para hallar la integral")
  } else{

    posInicial = 1
    posFinal = length(x)

    #Integral con un solo trapecio
    i1 = trapecio(x[posInicial], y[posInicial], x[posFinal], y[posFinal])
    #Integral con dos trapecios
    pivote = pivotear(posInicial, posFinal)
    i2 = trapecio(x[posInicial], y[posInicial], x[pivote], y[pivote]) + trapecio(x[pivote], y[pivote], x[posFinal], y[posFinal])
    #Integral con tres trapecios
    pivote1 = pivotear(posInicial, pivote)
    pivote2 = pivotear(pivote, posFinal)
    i3 = trapecio(x[posInicial], y[posInicial], x[pivote1], y[pivote1]) + trapecio(x[pivote1], y[pivote1], x[pivote], y[pivote]) + trapecio(x[pivote], y[pivote], x[pivote2], y[pivote2]) + trapecio(x[pivote2], y[pivote2], x[posFinal], y[posFinal])
  }

  i21 = ((4/3*i2)-(1/3*i1))
  i22 = ((4/3*i3)-(1/3*i2))

  final = ((16/15*i22)-(1/15*i21))

  cat("la integral definida de ", x[posInicial], "a", x[posFinal], " con los valores dados es de ", final)

  return (final)
}
