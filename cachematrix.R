############################ Lexical Scoping ###################################
#Name: María Fernanda López Ortega

## The following functions are used to cache and return the inverse of a matrix. 


## makeCacheMatrix creates a "special" matrix object that can cache its 
## inverse matrix.

makeCacheMatrix <- function(matriz = matrix()) {
  inversa <- NULL
  set <- function(matriz_c){
    matriz <<- matriz_c
    inversa <<- NULL
  }
  get <- function() matriz
  set_inversa <- function(solve) inversa <<- solve
  get_inversa <- function() inversa
  list(set = set, get = get,
       set_inversa = set_inversa,
       get_inversa = get_inversa)
}



## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse matrix has been calculated, then the
## cacheSolve should return the inverse matrix from cache.

cacheSolve <- function(matriz, ...){
  inversa <- matriz$get_inversa()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  data <- matriz$get()
  inversa <- solve(data, ...)
  matriz$set_inversa(inversa)
  inversa
}