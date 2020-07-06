## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return an object contening the methods to set,get both the matrix data and its inverse
makeCacheMatrix <- function(x = matrix()) {

    ## Initialize x_inverse to null and define set method
    x_inverse <- NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    ## define get method
    get <- function() x
    
    # define the set inverse method
    setinverse <- function(x_inverse_result) x_inverse <<- x_inverse_result
    # define the set get inverse method
    getinverse <- function() x_inverse
    # return object with all function handlers
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
## This function automatically retrieves the inverse of the matrix result. 
##If not present, calculate and store it on the object. 
cacheSolve <- function(x, ...) {
    ## Get inverse of the matrix
    x_inverse <- x$getinverse()
    ## if calculated previously, get it from objects cache
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    ## else, compute the matrix inverse and store it on objects cache
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$setinverse(x_inverse)
    x_inverse
}
