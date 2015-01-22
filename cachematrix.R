## The two functions work together to cache the inverse of a
## matrix to the parent environment of the makeCacheMatrix function.
## The example functions makeVector and cachemean from the 
## Assignment 2 web page were used heavily for inspiration to 
## create makeCacehMatrix and cacheSolve.

## makeCacheMatrix creates a list of objects stored in the environment

makeCacheMatrix <- function(x=matrix()){
    minv <- NULL
    set <- function(y){
          x <<- y
          minv <<- NULL
    }
    get <- function()x
    setminv <- function(solve) minv <<- solve
    getminv <- function()minv
    list(set=set, get=get, setminv=setminv, getminv=getminv)
}


## cacheSolve checks to see if the matrix inverse already exists.
## If it does, then it just returns the inverse of the matrix stored in the environment.
## If the inverted matrix does not exist, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...){
  minv <- x$getminv()
  if(!is.null(minv)){
      message("getting cached matrix inverse")
      return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
