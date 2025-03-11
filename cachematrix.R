## These functions work together to create a special "matrix" object 
## that can store its inverse, allowing for efficient retrieval of 
## the inverse if it has already been calculated.


## Creates a special "matrix" object that can cache its inverse,
## allowing for caching of the inverse computation.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Computes the inverse of the matrix returned by 'makeCacheMatrix'.
## If the inverse is already cached, it retrieves the cached result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
