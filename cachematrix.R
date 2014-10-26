# The purpose of these functions is (if the contents of a matrix are not changing)
# is to cache the value of the inverse so that when we need it again, 
# it can be looked up in the cache rather than recomputed. 

# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
        # set the inverse of the matrix
        # get the inverse of the matrix
        # set the value of the inverse
        # get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- list()
        length(m) <- length(x)
        dim(m) <- dim(x)
        m <- NULL
        set <- function(y) {
                x <<- y
                length(m) <- length(x)
                dim(m) <- dim(x)
                m <- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )     
                
}

# The following function calculates the inverse using the special "vector" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse and sets the value of the 
# inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
