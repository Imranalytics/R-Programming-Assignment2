## Matrix inversion is a time consuming computation so caching the inverse
## of a matrix is beneficial than computing it everytime. The functions
## in this program will cache the inverse of a matrix.

# # makeCacheMatrix will creat a list containing a function to
# # 1. set the value of the matrix
# # 2. get the value of the matrix
# # 3. set the value of inverse of the matrix
# # 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The below function will return the inverse of the matrix. I will first
## see if the inverse has already been computed. If it is computed, it
## will get the result and skip the computation. If it is not, then it will
## compute the inverse, and will set the value of inverse in the cashe
## through setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
