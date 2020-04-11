criba <- function(){
  n <- as.numeric(readline("Ingrese un número natural: "))
  if (n == 0 | n == 1){
    print(0)
  } else{
    X <- 2:n
    i <- 2
    for (x in X){
      if (2*x <= n){
        a <- seq(2*x,n,x)
        for (i in 1:length(a)){
          if (a[i] %in% X){
            b <- match(a[i],X)
            X <- X[-(b)]
          }
        }
      }
    }
    cat("Hay",length(X),"números primos desde 2 hasta",n,"y son \n",X)
  }
}