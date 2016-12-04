## makeCacheMatrix  do:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## This function creates a special "matrix" object that can cache its inverse. 
makeCacheMatrix <- function(M = matrix()) {
  solveM <- NULL
  set <- function(y) {
    M <<- y
    solveM <<- NULL
  }
  get <- function() M
  setSolve <- function(solve) solveM <<- solve
  getmean <- function() solveM
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## cacheSolve returns a matrix that is the inverse of 'M'
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(M, ...) {
  solveM <- M$getSolve()
  if(!is.null(solveM)) {
    message("getting cached data")
    return(solveM)
  }
  data <- M$get()
  solveM <- solve(data, ...)
  M$setSolve(solveM)
  solveM
        ## Return a matrix that is the inverse of 'M'
}
