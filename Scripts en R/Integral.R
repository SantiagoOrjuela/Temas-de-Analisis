integral <- function(){
  n <- as.numeric(readline("Ingrese el n�mero de rect�ngulos: "))
  a <- as.numeric(readline("Ingrese el l�mite inferior de la integral: "))
  b <- as.numeric(readline("Ingrese el l�mite superior de la integral: "))
  c<- abs(b-a)
  d <- min(a,b)
  i<-1:n; area <- sum((c/n)*exp(-(d+(c*i/n))^2))
  return(area)
}