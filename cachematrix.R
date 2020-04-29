

## create a funtion with null matrix argument

makeCacheMatrix <- function(x = matrix()) {
  ##initialise value of matrix inverse to null
  i<- NULL
  set <- function(y) {
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
    i <<- NULL
  }
  get <- function() x #function to get inverse
  setinv <- function(solve) i <<- solve #calculates the inverse of non-singular matrix via the solve function
  getinv <- function() i ### gets the inverse     
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## passes the value of the function makeCacheMatrix 
  
}


## function for finding cache of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) { ## find the inverse from cache if it's  there
    message("getting cached data-inverse of matirx")
    return(i)
  }
  ## if the inverse is not there in the cache it is first calculated and then retrieved.
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
