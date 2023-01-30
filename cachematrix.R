## The following shows the makeCacheMatrix and cacheSolve functions, which the first  creates a matrix object that can cache its inverse
## and the second calculates the inverse matrix returned by makeCacheMatrix().

#--------------------makeCacheMatrix-----------------------#
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmx <- NULL #Initialize the inverse property
    
  #Set the matrix
  set <- function(y) {
    x <<- y
    invmx <<- NULL
  }
  
  #The get the matrix
  get <- function() {
    x
  }
    
  setInverse <- function() {
    print("calculating inverse")
    invmx <<- solve(x) #calculate the inverse of the matrix
  }
   
  getInverse <- function() {
    invmx #Return the inverse
  }
  
  #Return the list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}
##End fuction makeCacheMatrix

#--------------------cacheSolve-----------------------#

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  invmx <- x$getInverse()
  if (!is.null(i)) {
    return(invmx)
  }
  
  #gets the matrix and stores it in m
  m <- x$get()
  invmx <- solve(m, ...)
  #Calculate inverse the matrix
  x$setInverse()
  invmx
}
##End function cacheSolve
