## Put comments here that give an overall description of what your
## functions do

##Try this:
## > a = c(2,3)
## > b = c(2,5)
## > abmatrix = matrix(c(a,b),byrow=TRUE,nrow=2)
## > mx <- makeCacheMatrix(abmatrix)
## > mx$get()
## > cacheSolve(mx)

## Write a short comment describing this function
## This Function will return a matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) 
            inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## This function will return the inverse of the first Matrix

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data.")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}
