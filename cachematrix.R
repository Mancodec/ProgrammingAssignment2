## The following two functions, makeCachematrix and cacheSolve, compute 
##and cache the inverse of a matrix. They both assume that the supplied
##matrix is invertible, i.e. that the matrix is square and its determinant
## is not zero.

## The following function, makeCacheMatrix, takes an invertible matrix 
## as input and returns a list contiaining functions to: 1.)set the
## value of the matrix; 2.) get the value of the matrix; 3.) set
## the value of the inverse; 4.) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize an empty matrix for the inverse
  xInverse <- matrix(nrow=dim(x)[1], ncol=dim(x)[2])
  
  ##function to cache matrix and its inverse
  set <- function(y){
        x <<-y
        xInverse <<- matrix(nrow=dim(x)[1], ncol=dim(x)[2])        
  }
  
  ## function to retreive matrix x
  get <- function() x
  
  ##function to compute the inverse
  setInverse <- function(Inverse) xInverse <<- solve
  
  ##function to retreive the inverse, xInverse
  getInverse <- function() xInverse
  
  ##Create and return the list of functions
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
  
}


## The following function computes the inverse of 
##the matrix, x created with the above function.
##However, it first checks to see if the inverse has 
##already been computed. If so,t gets the inverse 
##from the cache, skips the computation and returns
##the cached inverse. 
##Otherwise,it computes the inverse of the matrix
##caches the inverse via the setInverse function, then
## returns the inverse.

cacheSolve <- function(x, ...) {
  ## Check if an inverse for x has already been computed.
  xInverse <- x$getInverse()
  
  if(!is.na(xInverse[1,1])) {
    message("getting cached matrix inverse")
    return(xInverse)
  }
  
  workingMatrix <- x$get()
  xInverse <- solve(workingMatrix, ...)
  x$setInverse(xInverse)
  xInverse
}
