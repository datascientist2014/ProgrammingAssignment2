## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will use the similar functions as in the assignment to set, get, setinverse and getinverse a matrix input.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    latest<<-y
    if (isTRUE(all.equal(x,y))){message("Matrices are same, so not changing m")} 
    else {message("Matrices not same, hence setting m to NULL")
          m <<- NULL}
  }
  get <- function() x
  setinverse <- function(x) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a similar function as in tutorial but computes inverse only if the matrix is not same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  #print(inv)
  if(!is.null(inv)) {
    message("Now Printing cached Inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  message("No Cache Avaiable. Computed Inverse Now")
  inv
}
}
