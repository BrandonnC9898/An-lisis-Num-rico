richardson = function(x,y,h,n){
  require(PolynomF)
  lagrange = function(x,y,a){
    n = length(x)
    if(a < min(x) || max(x) < a) stop("No est? interpolando")
    X = matrix(rep(x, times=n), n, n, byrow=T)
    mN = a - X; diag(mN) = 1
    mD = X - t(X); diag(mD) = 1
    Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
    sum(y*Lnk)
  }

  cinco2 = function(x,y,x0,h){
    return((1/(2*h))*(interpolacion(x,y,(x0+h))-interpolacion(x,y,(x0-h))))
  }

  interpolacion = function(x,y,x0){
    return(lagrange(x,y,x0))
  }

  richardsonH = function(x,y,x0,h,n){
    if(n == 1){
      return(c(cinco2(x,y,x0,h),0))
    }else{
      n1 = richardsonH(x,y,x0,(h/2),n-1)
      n0 = richardsonH(x,y,x0,h,n-1)
      r = n1[1]+((n1[1]-n0[1])/(4^(n0[1]-1)-1))
      e = (abs(n1[1]-n0[1]))/(4^(n0[1]-1)-1)
    }
    return(c(r,e))
  }

  cat("x\t\ty\t\tR\t\te\n")
  for(i in 2:(length(x)-1)){
    cat(x[i],"\t",y[i],"\t",richardsonH(x,y,x[i],h,n),"\n")
  }
}
