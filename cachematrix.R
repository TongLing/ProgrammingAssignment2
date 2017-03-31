
# Matrix inversion is usually a costly computation and there may 
# be some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to 
# write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <- function(y){
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


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
          
# > source("makeCacheMatrix.R")
# > x <- rbind(c(0.5, -1/4), c(-1/4, 0.))
# > x <- rbind(c(0.5, -1/4), c(-1/4, 0.5))
# > m <- makeCacheMatrix(x)
# > m$get()
#       [,1]  [,2]
# [1,]  0.50 -0.25
# [2,] -0.25  0.50
# > cacheSolve(m)
#          [,1]     [,2]
# [1,] 2.666667 1.333333
# [2,] 1.333333 2.666667
# > cacheSolve(m)
# getting cached data.
# [,1]     [,2]
# [1,] 2.666667 1.333333
# [2,] 1.333333 2.666667
