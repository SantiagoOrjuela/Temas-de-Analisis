integral <- function(){
  n <- as.numeric(readline("Ingrese el número de rectángulos: "))
  a <- as.numeric(readline("Ingrese el límite inferior de la integral: "))
  b <- as.numeric(readline("Ingrese el límite superior de la integral: "))
  c<- abs(b-a)
  d <- min(a,b)
  i<-1:n; area <- sum((c/n)*exp(-(d+(c*i/n))^2))
  return(area)
}