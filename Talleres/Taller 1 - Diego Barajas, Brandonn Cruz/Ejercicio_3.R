raiz <- function(n, e, x){
  y <- (x+(n/x))/2
  while (abs(x-y) > e) {
    x <- y
    y <- (x+(n/x))/2
  }
  return(y)
}

print(raiz(7,0.0001,2.3333))
print(raiz(16,0.0001,3.8))