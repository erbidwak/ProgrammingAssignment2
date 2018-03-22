## This program receives a matrix and cache it inverse. 
## It also checks if the inverve of the matrix is in the cache and retrieve it if needed

## This function receives a matric x and set or get it inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
  
}


## This function first checks if the inverse exists if yes, then load it. Otherwise it inverses
## the matrix and cache it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Loading data from cach")
    return(inv)
  }
  m <- x$get()
  
  ## inverse the matrix
  inv <- solve(m,...)
  x$setInverse(inv)
  inv
}
