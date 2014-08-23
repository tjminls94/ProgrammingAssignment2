## Function makeCacheMatrix creates a matrix object that can cache
## it's inverse.

## Function cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        ## Input a matrix, create a special vector object that
        ## is a list of the value of the matrix, the cached
        ## value of the inverse.
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- null
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.  If the inverse
        ## has already been calculated, then the value of "s" will not
        ## be null, and the function deliver a message that it is
        ## getting the cached value, and will return the cached value.
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data,...)
        x$setsolve(s)
        s
        
}
