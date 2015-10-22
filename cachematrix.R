## The assignment is to write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse
# 1 - set the value of the matrix
# 2 - get the value of the matrix
# 3 - set the value of the inverse
# 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}

# The function below will return the inverse of the matrix:
# 1 - Chek if the inverse has been computed already
# 2 - If has been computed it will get the result and skip the computation
# 3 - If hasn't been computed, it will compute the inverse of the matrix.
# 4 - And will set the value in the cache via the setinverse function.



## This function assumes that the underlined matrix is always invertible

cacheSolve <- function(x, ...) {
  
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cache data.")
    return(Inv)
    
  }
  data <- x$get()
  Inv <- solve(data)
  x$setinverse(Inv)
  Inv
        
}
