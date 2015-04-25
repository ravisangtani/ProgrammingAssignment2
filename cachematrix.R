## The below code contains two functions

## function "makecachematrix" will cache the results of the inverse of the matrix so that if an input is given, the function caches 
## the results of the input which can be reused if inetarive operations where input doesnt change 

## Function cachesolve take input argument a object to which "makecachematrix" has been assigned.  This function checks to see if
## the input provided already has the results stored in which case it fetches the results from cache, else it recalculates the
## inverse again


#first code to cache the results
###########

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



#second code to pull the results or recalculate inverse again
###########

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}