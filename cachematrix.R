## makeCacheMatrix takes a matrix as an argument and creates a function object (list) containing
## functions to:
## create (set) the matrix
## retreive (get) the matrix
## store (set) the calculated inverse of the matrix 
## retreive (get) the calculcated inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inverseOfMatrix <- NULL
      set <- function(matrixToSet) {
            x <<- matrixToSet
            inverseOfMatrix <<- NULL
      }
      get <- function() {
            return(x)
      }
      setinverse <- function(solvedInverseOfMatrix) { 
            inverseOfMatrix <<- solvedInverseOfMatrix 
      }
      getinverse <- function() { 
            return(inverseOfMatrix)
      }
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes a matrix function object and evaluates if an inverse has been calculated (set) by first
## retreiving the inverse function value and if this is not null, return the function value back to the caller.
## If the inverse function value was null, the function calculates the inverse by solve(matrix) 
## and sets the inverse value on the matrix object
cacheSolve <- function(x, ...){
      inverseOfMatrix <- x$getinverse()
      if(!is.null(inverseOfMatrix)) {
            message("getting cached inverse of matrix")
            return(inverseOfMatrix)
      }
      matrix <- x$get()
      inverseOfMatrix <- solve(matrix, ...)
      x$setinverse(inverseOfMatrix)      
      return(inverseOfMatrix)
}
