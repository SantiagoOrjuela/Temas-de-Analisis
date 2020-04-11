diagonal <- function(M){
  m <- length(M[1,])
  n <- length(M[,1])
  D <- matrix(0,n,m)
  for (i in 1:m){
    for(j in 1:n){
      if (i == j){
        D[i,j] <- M[i,j]
      }
    }
  }
  print(D)
}

superior <- function(M){
  m <- length(M[1,])
  n <- length(M[,1])
  U <- matrix(0,n,m)
  for (i in 1:m){
    for (j in 1:n){
      if (i < j){
        U[i,j] <- M[i,j]
      }
    }
  }
  print(U)
}

inferior <- function(M){
  m <- length(M[1,])
  n <- length(M[,1])
  L <- matrix(0,n,m)
  for (i in 1:m){
    for (j in 1:n){
      if (i > j){
        L[i,j] <- M[i,j]
      }
    }
  }
  print(L)
}

invdia <- function(M){
  m <- length(M[1,])
  n <- length(M[,1])
  D1 <- matrix(0,n,m)
  for (i in 1:m){
    for (j in 1:n){
      if (i == j & M[i,j] != 0){
        D1[i,j] <- 1/M[i,j]
      }
    }
  }
  print(D1)
}

norma <- function(x){
  i <- 1:length(x); N <- sqrt(sum(x[i]^2))
  return(N)
}

Jacobi <- function(A,B){
  k <- as.numeric(readline("Ingrese el número de iteraciones:"))
  m <- length(A[1,])
  L <- inferior(A)
  U <- superior(A)
  D <- diagonal(A)
  D1 <- invdia(D)
  Tj <- D1 %*% (L+U)
  lI <- diag(m)
  C <- D1 %*% B
  x <- integer(m)
  it <- matrix(0,k,m)
  a <- eigen(A)
  b <- eigen(Tj)
  tol <- 0
  for(i in 1:k){
    it[i,] <- C- Tj %*% x
    x <- C - Tj %*% x
    if (i == 1){
      tol <- 1
    } else{
      tol <- norma(it[i,]-it[i-1,])/norma(it[i,])
    }
    print(tol)
  }
  print(it)
  print(a)
  print(b)
}