## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #Make sure x is a matrix
  if(!is.matrix(x)) stop("x must be a matrix")
  mi <- NULL
  set <- function(y) {
    x <<- y
    mi <<- NULL
  }
  get <- function() x
  setMatInv <- function(solve) mi <<- solve
  getMatInv <- function() mi
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getMatInv()
  #if the matrix inverse exists, get cached value
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  #need to calculate inverse
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  #cache the inverse for future usage
  x$setMatInv(matrixinverse)
  matrixinverse
}
