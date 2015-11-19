#####################
## makeCacheMatrix ##
#####################

## makeCacheMatrix makes a list of functions in which. The matrix given is stored in the function's environment.
## get: returns the stored matrix
## set: changes the stored matrix and resets the inverse matrix
## setinverse: stores the inverse matrix (in variable i)
## getinverse: returns the stored inverse of the matrix


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

########################
## cacheSove function ##
########################

## x is the special matrix created with the makeCacheMatrix function
## example:
## cm <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(cm) 

## if calling cacheSolve the function checks if the inverse has already been put in the 
## function (makeCacheMAtrix) environment. 
## if so the cached inverse matrix is returned and a message is displayed.

## use the ... argument to pass other arguments to the solve() function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return the cached inverse of 'x' when it has already been computed
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverted matrix")
    return(i)
  }
  ## next part runs if matrix was not cached yet.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
