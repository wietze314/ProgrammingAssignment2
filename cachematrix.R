## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores the matrix in the in the function environment in the list 
## that is the result of makeCacheMatrix


## if so the getting cached data message is thrown and the ufnction returns i
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    ## reset inverse to NULL when matrix is changed
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) i <<- matrixinverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

## x is the special matrix
## example:
## cm <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(cm) 

## if calling cacheSolve the function checks if the inverse has already been put in the 
## function (makeCacheMAtrix) environment (as variable x). X is stored in i in the environment of function cacheSolve.

## use the ... argument to pass other arguments to the Solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
