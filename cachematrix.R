## Two functions that allow a matrix's inverse to be cached and retrieved.

## The first function creates a "class" (special list) that allows values 
## to be set and retrieved.

## The second performs the actual caching and retrieval, testing for whether 
## or not the inverse has previously been stored.


# Initial function which sets up a special object structure to hold a cached 
# matrix inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    
    # Define four functions which allow the original matrix data to be set and 
    # retrieved, and also allow its inverse to be set and retrieved. Whenever 
    # new matrix data is stored, set the inverse to NULL.
    
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    # Create the list object that holds the above functions for 
    # getting / setting the data and the inverse.
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


# Second function which returns the inverse of a "cacheMatrix" list as defined 
# in the first function above. If the inverse has already been cached, return 
# the cached data instead of recalculating the inverse.

cacheSolve <- function(x, ...) 
{
    # Get the current stored inverse of the matrix
    
    inv <- x$getinverse()
    
    # Check whether the current inverse has been calculated. If so, return the 
    # cached data. 
    
    if(!is.null(inv)) 
    {
        # [debug printout] message("getting cached inverse data")
        return(inv)
    }
    
    # If the inverse has not been stored previously, calculate it and cache 
    # the new value before returning it.
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    
    inv
}
