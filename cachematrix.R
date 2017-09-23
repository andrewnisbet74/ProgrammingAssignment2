

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL
       # Sets the matrix to the value in y and resets the inverse to NULL
       set <- function(y) {     
          x <<- y
          inverse <<- NULL
       }

       # Returns the matrix 
       get <- function()  x    
       setInverse <- function(i)  inverse <<- i
       getInverse <- function() inverse 
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Comptures the inverse of the provided matrix, or returns a cached version of the computation already exectued. 
# Solve method in R docs(https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/solve)
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        # See if the inverse has already been calculated.
        inverse <- x$getInverse()

        # check if the inverse is NULL, if it is not then it has already beeen calculated and we can just return 
        # the cached value
        if (!is.null(inverse)) {
           message("getting cached data")
           return(inverse)
        }
        
        # If the value is NULL, we need to cacluate it using solve, and then cache it.  
        data <- x$get()
        message(" Calculating Inverse, and caching results")
        inverse  <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
