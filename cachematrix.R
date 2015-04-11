## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix: This function creates a special 
##                   "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## setting the cache value
  cacheMatrix <- NULL
  
  ## setter
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  ## getter
  get <- function() x
  
  ## cache set
  setMatrix <- function(m) cacheMatrix <<- m
  
  ## cache get
  getMatrix <- function() cacheMatrix
  
  ## return methods
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve: compute the inverse matrix if not cached before.
##             retrieve otherwise

cacheSolve <- function(x, ...) {
  
  ## retrieve the cache
  cacheMatrix <- x$getMatrix()
  
  ## verify if the cache exists
  if(!is.null(cacheMatrix)) {
    
    ## ends the method with return
    return(cacheMatrix)
  }
  
  ## get the data
  matrixData <- x$get()
  
  ## compute the inverse matrix
  cacheMatrix <- solve(matrixData)
  
  ## set the inverse matrix to the cache
  x$setMatrix(cacheMatrix)
  
  ## return the cache matrix
  cacheMatrix
}
