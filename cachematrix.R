
#This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinversa <- function(inversa) invmatrix <<- inversa
  getinversa <- function() {invmatrix}
  cachelist <- list(set = set, get = get,
                    setinversa =  setinversa,
                    getinversa = getinversa)
}

#  This function computes the inverse of the matrix returned by makeCacheMatrix 
#  or returns de cache of the matrix (with a message)
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinversa()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  mat <- x$get()
  invmatrix <- solve(mat, ...)
  x$setinversa(invmatrix)
  invmatrix
}

#Test

testmatrix <- matrix(5:8,2,2)
specialmatrix <- makeCacheMatrix (testmatrix)
cacheSolve (specialmatrix)
