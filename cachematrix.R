## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse = matrix()) 
                inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`
cacheSolve <- function(x, ...) {
##	try to get the cached inverse from x
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting chached data")
                return(inv)
        }
##	otherwise compute it and store it in x
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
        inv
}
