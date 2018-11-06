#  cachematrix.R

# 2 functions
# makeCacheMatrix
#   Args:
#       x (matrix) : must be square invertible matrix
#   Return:
#       special "vector", which is really a list containing a function to
#           set        : set the value of the vector
#           get        : get the value of the vector
#           setinverse : the value of the inverse
#           getinverse : the value of the inverse

# cacheSolve
#   Args:
#       x (object)  : created by makeCacheMatrix function
#   Return: 
#       inverse of the matrix stored by x, will set if not already saved


makeCacheMatrix <- function(x = matrix()) {
    ## Create a cached object from an inversable matrix
    cache_inverse <- NULL
        set <- function(y) {
            x <<- y
            cache_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cache_inverse <<- inverse
        getinverse <- function() cache_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return the inverse of an object created by makeCacheMatrix
    ##             that is the inverse of arg 'x'
    
    cache_inverse <- x$getinverse()
    if(!is.null(cache_inverse)) {
        message("getting cached data")
        return(cache_inverse)
    }
    data <- x$get()
    cache_inverse <- solve(data, ...)
    x$setinverse(cache_inverse)
    cache_inverse
    
}
