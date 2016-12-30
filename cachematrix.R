## R Programming Assignment 2
## 2016 December
## 
## These function take the inverse of a matrix and cache the result making
## it available for recall thereby using less system resources.
##
## This function takes a matrix and caches in inverse of the matrix along
## with the function environment

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL 
   set <- function(y) {
    x <<- y
    m <<- NULL 
  } # function(y) end
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
} # makeCacheMatrix function end


## This function returns the inverse of the matrix previously cached if it is
## present, otherwise, it calculates the inverse on its own.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m ## Return a matrix that is the inverse of 'x'
} # end of cacheSolve
