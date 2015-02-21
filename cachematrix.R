@@ -3,13 +3,45 @@

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}

# x = rbind(c(1, 2), c(2, 1))
# inv = makeCacheMatrix(x)
# inv$get()
# cacheSolve(inv)
