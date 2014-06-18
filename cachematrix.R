## These methods can be used to cache the inverse calculation.
## to create a cachable matrix, do this:
## cachedMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## to calculate the inverse do this:
## cacheSolve(cachedMatrix)


## This function generates a cacheMatrix object that holds a matrix and it's
## possibily cached inverse. In order to calculate the inverse using caching
## use the cacheSolve function.
## To construct a cachedMatrix, pass a matrix:
## cachedMatrix <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of a CacheMatrix 
## which was constructed my using the makeCacheMatrix function.
## The First call calculates the inverse, every succeeding call
## uses the cached inverse.
## To calculate the inverse pass the cacheMatrix object:
## cacheSolve(cachedMatrix)

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
