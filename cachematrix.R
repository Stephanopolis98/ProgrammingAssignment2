#RProgramming Assignment 2 - Functions that cache the inverse of a matrix


#This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL #initialize the value of the inverse matrix to NULL (restarts everything)
  set <- function(y) { #sets the value of the matrix using other function
    x <<- y             
    invmatrix <<- NULL
  }
  get <- function() x #value that will return the matrix
  setinversa <- function(inversa) invmatrix <<- inversa #sets the inverse of the matrix
  getinversa <- function() {invmatrix} #value that will return the inverse of the matrix
  list(set = set, get = get,
                    setinversa =  setinversa,
                    getinversa = getinversa)
}

#  This function computes the inverse of the matrix returned by makeCacheMatrix 
#  or returns de cache of the matrix (with a message)
cacheSolve <- function(x, ...) {
  invmatrix <- x$getinversa() #get the already calculated inversed matrix (if possible)
  if(!is.null(invmatrix)) { ##If the inverse was already calculated, 
    message("getting cached data") ##the inversed matrix is returned with a message
    return(invmatrix)
  }
  mat <- x$get()            ## If it wasn't calculated, then the inverse is calculated using the
  invmatrix <- solve(mat, ...) ## matrix created by "makeCacheMatrix" with solve().
  x$setinversa(invmatrix)        ##It is assumed that the matrix supplied is always invertible
  invmatrix 
}

#Test

testmatrix <- matrix(5:8,2,2)
specialmatrix <- makeCacheMatrix (testmatrix)
cacheSolve (specialmatrix)
