##This function creates a special "matrix" object that can cache its inverse for the input (which is an invertible square matrix)

##Set the input 'x' as a matrix, set the solved value 'inv' as a null and set the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {         
    x <<- y
    inv <<- NULL
  }
  
  ##Get the value of the matrix, set the value of the inverse and get the value of the inverse
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),then it retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()     #Here, return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##Set the value of the inverse of the cache, compute the inverse of the matrix and set the value of the inverse of the cache
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}