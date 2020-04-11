raices <- function(){
  a <- as.numeric(readline("a = ")) 
  b <- as.numeric(readline("b = "))
  c <- as.numeric(readline("c = "))
  d <- mpfr(b^2-4*a*c,64)
  
  if(a==0) {stop("No es cuadrática")}
  
  if(d==0){
    x1 <- -b/(2*a)
    cat("Una raíz real x1 = ", x1)
  }
  if(d>0){
    x1 <- (-b+sqrt(d))/(2*a)
    x2 <- (-b-sqrt(d))/(2*a)
    print(x1)
    print(x2)
  }
  if(d<0){ cat("Las raíces son complejas")
  }
}