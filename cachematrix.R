## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Define the set function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix is changed
    }
    
    # Define the get function to retrieve the matrix
    get <- function() x
    
    # Define the setinverse function to cache the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Define the getinverse function to retrieve the cached inverse
    getinverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse
    inv <- x$getinverse()
    
    # If the inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setinverse(inv)
    
    # Return the inverse
    inv
}
