## This fanction creates a special "matrix" object that can cache its inverse object

## This function creates a special "matrix", which is really a list containing a functions to :
## set the value of the vector - get
## get the value of the vector - set
## set the inverse matrix - setinvmx
## get the inverse matrix - getinvmx

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
