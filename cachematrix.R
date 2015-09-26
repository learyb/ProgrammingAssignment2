## Put comments here that give an overall description of what your functions do
## makeCacheMatrix will return a list containing 4 sub functions which are ultimately invoked by the cacheSolve function
## cascheSolve will determine if we have already calculated the inverse of the target matrix and return that result
## if not, it will invoke the function to calculate and store the inverted matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x<<- y
    m<<- NULL
  }
  get <- function() {x}
  setinv <- function(invertit) m <<- solve(x)
  getinv <- function() m 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) { 
  m <- x$getinv()
  if(!is.null(m)) {message("using already calc'd and cached data / results")
    return(m)
  }
##  data <- x$get()
##  m <- solve(data)
  m <- solve(x$get())
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

## Write a short comment describing this function
