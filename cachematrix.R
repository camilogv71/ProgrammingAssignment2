## This function creates a special matrix containing a list
## whit the functions get, set, setinverse and getinverse
## Is the same as the on provided in the instructions, but with the
## inverse instead of the mean.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    #sets the value of the matrix to a variable on a different environment
    x <<- y
    m <<- NULL
  }
  get <- function() x   #gets the value of the matrix
  setinverse <- function(inverse) m <<- inverse   #sets the value of the inverse
  getinverse <- function() m #returns the value of the matrix
  list(set = set, get = get,    #creates the list
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix
## If the value was already calculated, it will return the cached value
## If its the first time calculating the matrix, calculates the inverse, stores the value for future use, and returns the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()    #Gets the inverse form the cache
  if(!is.null(m)) {  #If its not null, gets the data without computing the inverse
    message("getting cached data")
    return(m)
  }
  data <- x$get() #If its null, it gets the matrix, calculates the inverse and returns it
  m <- solve(data)
  x$setinverse(m)
  m
}
