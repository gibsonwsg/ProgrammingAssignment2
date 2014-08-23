## The following functions interact to generate and cache
## the inverse of a matrix argument. 


## The makeCacheMatrix function provides several methods to manage 
## the matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL  # Store mean value, resetting on every 
  #function invocation
  
  set <- function(y) {    # assigns values to higher level variables
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  # return value of original vector
  
  setInv <- function(solve) m <<- solve  # called by cacheSolve on first
  # iteration of solve()
  
  getInv <- function() m  # returns cached value
  
  list(set = set, get = get, # return list of function available to 
       setInv = setInv ,     # to external functions
       getInv  = getInv )
}


## The cacheSolve function returns the cached value
## of previously computed matrix inverses.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()  # accesses the stored inverse value 
  # of the matrix
  
  if(!is.null(inv)) {   # returns the cached inverse value if it
    # already exists
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get() 
  
  inv <- solve(data, ...) # calculate the inverse of the matrix
  
  x$setInv(inv)
  
  inv    # return the inverse 
}

