suma_parcial <- function(){
  k <- as.numeric(readline("Ingrese un n�mero entero positivo: "))
  i<-1:k; a<-sum(1/(i ^2))
  return(a)
} 