## Large Matrix inversion is  a time-consuming computation. 
## We can cache  the inverse of the matrix rather than extra computation.
## you can test these two functions with the following test scripts
##The first time you try to do this ,you can get the message "This is the first time to get the inverse of this matrix and we get it cached "
##m <- makeCacheMatrix(m = matrix(c(2,0,0,5),nrow=2,ncol=2))
##cacheSolve(m)
##The second time you try yo do this agaign,you will get the message "oops,you have a cached one"
##cacheSolve(m)


## Creates a special matrix with the ability to cache its inverse.
##
## set() creates the matrix
## get() retrieves the existing matrix
## setinverse() caches the matrix inverse computed by cacheSolve
## getinverse() retrieves the existing matrix inverse or NULL
makeCacheMatrix <- function(m = matrix()) {
  inverse_cached <- NULL
  set <- function(newMatrix) {
    m <<- newMatrix
    inverse_cached <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inverse_cached <<- inverse
  getinverse <- function() inverse_cached
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## For a special invertible matrix,you can cache it inverse with calling the function makeCacheMatrix().
## Usually you can use the solve() function .Instead here you can get the inverse with calling the function cacheSolve().
## If the matrix inverse has already been calculated then return the cached value. 
## Otherwise, calculate the matrix inverse and return it.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_cached <- m$getinverse()
  if (!is.null(inverse_cached)) {
    message("oops,you have a cached one")
    return(inverse_cached)
  }
  data <- m$get()
  inverse <- solve(data, ...)
  m$setinverse(inverse)
  message("This is the first time to get the inverse of this matrix and we get it cached ")
  inverse
}