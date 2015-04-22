## Put comments here that give an overall description of what your
## functions do

## this fanction creates a special "matrix" object that can cache its inverse object

makeCacheMatrix <- function(x = matrix()) {

  
  invmx <- NULL
  set <- function(y) {
    x <<- y
    invmx <<- NULL
  }
  get <- function() x
  setinvmx <- function(p_invmx) invmx <<- p_invmx
  getinvmx <- function() invmx
  list(set = set, get = get,
       setinvmx = setinvmx,
       getinvmx = getinvmx)  
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmx <- x$getinvmx()
  if(!is.null(invmx)) {
    message("getting cached inverse matrix")
    return(invmx)
  }
  data <- x$get()
  invmx <- solve(data, ...)
  x$setinvmx(invmx)
  invmx        
  
  
}
