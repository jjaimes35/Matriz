# # La funci?n makeCacheMatrix almacena en cach? la inversa de una matriz y la funci?n
# # cacheSolve calcula la inversa de la matriz

# # La siguiente funci?n makeCacheMatrix crea un objeto "matriz" especial que puede
# # cach? su inverso.

makeCacheMatrix <- function(x = matrix()){
  inversa <- NULL
  set <- function(y){
    x<<-y
    inversa<<-NULL
    
  }
  get<-function(){x}
  setInversa<- function(inversacalculada){inversa<<-inversacalculada}
  getInversa<- function(){inversa}
  list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}

# # La siguiente funci?n cacheSolve calcula la inversa de la "matriz" especial
# # devuelto por makeCacheMatrix arriba.

cacheSolve <- function(x, ...) {
  inversa <- x$getInversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInversa(inversa)
  inversa
}

