## makeCacheMatrix takes a matrix as an argument and creates a function object (list) containing
## functions to:
## create (set) the matrix (initializes the matrix value and sets the calculated inverse of the matrix to NULL
## retreive (get) the matrix
## store (set) the calculated inverse of the matrix 
## retreive (get) the calculcated inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {      
      inverseOfMatrix <- NULL      
      set <- function(matrixToSet) {
            x <<- matrixToSet
            inverseOfMatrix <<- NULL
      }      
      get <- function() { return(x) }      
      setinverse <- function(calculatedInverseOfMatrix) { inverseOfMatrix <<- calculatedInverseOfMatrix }      
      getinverse <- function() { return(inverseOfMatrix) }      
      return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## cacheSolve takes a matrix function object and evaluates if an inverse has been calculated (set) by first
## retreiving the inverse matrix value. If this value is null, the function calculates the inverse by solve(matrix) 
## and sets the inverse value on the matrix function object. The function then returns the inverse value.
cacheSolve <- function(x, ...){
      inverseOfMatrix <- x$getinverse()
      if(is.null(inverseOfMatrix)) {       
            matrix <- x$get()
            inverseOfMatrix <- solve(matrix, ...)
            x$setinverse(inverseOfMatrix)            
      }
      return(inverseOfMatrix)
}
