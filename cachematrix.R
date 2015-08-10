## cachematrix.R contains two functions, makeCacheMatrix and cacheSolve.
## These functions cache the inverse of a matrix, assess whether there as an 
## inverse matrix cached then retrieve it if so and calculate it if not.

## For a matrix x (default x=matrix())
## a <- makeCacheMatrix(x)
## b <- cacheSolve(a)

## makeCacheMatrix creates a list of four fuctions.
makeCacheMatrix <- function(x = matrix()) {
  ## Set the object that will store the cached inverse of the matrix to null
  m <- NULL

  ## Set the value of the matrix
  setmatrix <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  ## Get the value of the matrix x
  getmatrix <- function() x
  
  ## Set the value of the inverse - in the global environment
  ##  solve() is the inbuilt R function to get the inverse of a matrix
  setinverse <<- function(solve) m <<- solve

  ## Get the value of the inverse - in the global environment
  getinverse <<- function() m

  # Put the four functions together in a list
  list(setmatrix=setmatrix, getmatrix=getmatrix,
       setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is passed the list of functions from makeCacheMatrix as x.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cacheSolve retrieves the inverse from the cache.
## It assumes the matrix can always be inverted.
cacheSolve <- function(x, ...) {
        ## Retrieves the inverse matrix from the cache.
        ## m will be NULL if there is no inverse matrix cached.
        m <- x$getinverse()
        
        ## Checks whether there is a cached inverse matrix.
        ## Returns it if there is
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        ## Retrieves the original matrix as specified as x in makeCacheMatrix.
        originalMatrix <- x$getmatrix()
        
        ## Calculates the inverse of the original matrix using the inbuilt R function.
        m <- solve(originalMatrix)
        
        ## Sets the output matrix to the inverse
        x$setmatrix(m)
        
        ## Returns the value of m, the inverse matrix
        m
}

# HJS 10/08/2015
