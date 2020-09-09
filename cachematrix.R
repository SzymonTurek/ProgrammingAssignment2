## Put comments here that give an overall description of what your
## functions do
#input x is a matrix and invers is my inversed result
makeCacheMatrix <- function(x = matrix()){
      invers <- NULL
      set <- function(y){
            x <<- y
            invers <<- NULL
      }
      get <- function() {x}
      set_inv <- function(inverse) {invers <<- inverse}
      get_inv <- function() {invers}
      list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

cacheSolve <- function(x, ...){
      invers <- x$get_inv()
      if(!is.null(inv)){
            return(invers)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$set_inv(invers)
      invers
}
