## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(0, 2, 2, 0), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5  0.0

## Create a special "matrix", which contains all the listed criterions 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Make inverse of the special "matrix" 
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
