## This asignment aims to write a pair of functions 
## that cache the inverse of a matrix.

## This function creates a special "matrix" object 
##that can cache its inverse.


makeCacheMatrix = function (x=matrix()) {
  ix <- matrix()
  setMatrix <- function(y) {
    x <<- y
    ix <<- matrix()
  }
  getMatrix <- function() x
  setInvertMatrix <- function(InvertMatrix) ix <<- InvertMatrix
  getInvertMatrix <- function() ix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvertMatrix = setInvertMatrix,
       getInvertMatrix = getInvertMatrix)
}  

## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    ix <- x$getInvertMatrix()
    if((nrow(ix)==1) & (ncol(ix)==1) & (is.na(ix[1,1]))) {
      data <- x$getMatrix()
      ix <- solve(data, ...)
      x$setInvertMatrix(ix)
      ix
    }  
    else {  
      message("getting cached Inverte Matrix")
      return(ix)
    }
    
  }