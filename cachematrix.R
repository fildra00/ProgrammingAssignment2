##Creating two functions that allow for a "faster computation" of an inverse of a matrix. 
# The first one allows to store the inverse of the matrix as a special object 
# and the second one calculates the inverse values 
# but only if this has not been previously executed, otherwise, it gets the value from the cache.  

## 1st Function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## 2nd Function computes the inverse of the matrix, if already computed then retrieves the value from the memory. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
      i
}

