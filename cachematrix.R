
# The function, makeCacheMatrix creates a special "matrix" object that can cache
# its inverse. return a list with containing a function to 
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    # Clear the cache
    inv <- NULL
    set <- function(y) {
        x <<- y                    # Set the value of matrix
        inv <<- NULL               # Clear the cache
    }
    
    # Get the value of matrix
    get <- function() x            
    
    # Set the value of the inverse matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the value of the inverse matrix
    getinverse <- function() inv
    
    # Return a list with the four functions
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

# Function cacheSolve: Compute the inverse of the matrix.
# If the inverse is already calculated before, it returns the cached inverse.
#
# @params X is a list created with makeCacheMatrix function 
#

cacheSolve <- function(x, ...) {
        
    # Get inverse matrix cached
    inverseFunction <- x$getinverse()
    
    # Check if exist the inverse matrix cached, and if exist return
    if(!is.null(inverseFunction)) {
        message("getting cached data")
        return(inverseFunction)
    }
    # get value of matrix
    data <- x$get()
    
    # Calculate Inverse matrix
    inverseFunction <- solve(data, ...) # Calculate inverse matrix
    
    # cache the inverse matrix
    x$setinverse(inverseFunction)
    
    # return inverse matrix
    inverseFunction
}