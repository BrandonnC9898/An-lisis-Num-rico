integral <- function(x, y){

  maxTrape <- function(tam){
    if(tam <= 2){
      return (1)
    }
    else{
      if((tam %% 2) == 0){
        paso = (tam/2)
      }
      else{
        paso = ((tam+1)/2)
      }
      return(1 + maxTrape(paso))
    }
  }

  pasoPivote <- function(cantiParticiones, tamDatos){
    while(cantiParticiones>1){
      if((tamDatos %% 2) == 0){
        tamDatos = (tamDatos/2)
      }
      else{
        tamDatos = ((tamDatos+1)/2)
      }
      cantiParticiones = cantiParticiones - 1
    }
    return(tamDatos-1)
  }

  trapecio <- function(x1, y1, x2, y2){
    h = x2-x1
    area = (h/2)*((y1+y2))
    return(area)
  }

  integralApro <- function (nivel, tam, x, y){
    total = 0
    paso = pasoPivote(nivel, tam)
    trap = seq(1, tam, by = paso)
    anterior = -1
    for (i in trap){
      if(anterior == -1){
        anterior = i
      }
      else{
        total = total + trapecio(x[anterior], y[anterior], x[i], y[i])
        anterior = i
      }
    }
    return(total)
  }

  if(length(x) != length(y)){
    stop("Los valores para 'Y' no coinciden con los valores en 'X'")
  } else if(length(x) < 3){
    stop("Se nesesitan mas valores para hallar la integral")
  } else{

    particiones = maxTrape(length(x))
    inte = seq(1, particiones, by=1)
    for (i in 1:particiones) {
      inte[i] = integralApro(i, length(x), x, y)
    }
  }

  canti = particiones
  nivel = 2
  while(canti > 1){
    for (i in 1:canti-1) {
      inte[i] = (((4^(nivel-1))/((4^(nivel-1))-1))*inte[i+1])-((1/((4^(nivel-1))-1))*(inte[i]))
    }
    canti = canti -1
    nivel = nivel +1
  }
  return(inte[1])

}


