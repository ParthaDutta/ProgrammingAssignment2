# "cacheSolve" calculates the inverse of a square matrix
# assume calculating the inverse is an expensive operation, the inverse is calculated only once and stored in "makeCacheMatrix"



# "makeCacheMatrix" holds the inital matrix and when calculated the inverse
# getters and setters are provided for both

makeCacheMatrix <- function(rawValue = matrix()) {

  cachedValue <- NULL
  
  setMatrix <- function(value) {
    rawValue    <<- value
    cachedValue <<- NULL
  }
  getMatrix        <- function() rawValue
  setMatrixInverse <- function(value) cachedValue <<- value  
  getMatrixInverse <- function() cachedValue

  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

# "cacheSolve" calculated the inverse
# input parameter is the function "makeCacheMatrix"
# "makeCacheMatrix" needs to be populated with a matrix first before being passed in
# first check if inverse has already been calculated, if so return inverse, no need to calculate
# if not get matrix from "makeCacheMatrix" and calculate inverse
# store inverse back in "makeCacheMatrix" and then return inverse

cacheSolve <- function(x, ...) {
  
  local <- x$getMatrixInverse()

  if(!is.null(local)) {
    message("getting cached data")
    return(local)
  }

  local <- solve(x$getMatrix(), ...)

  x$setMatrixInverse(local)

  local
}