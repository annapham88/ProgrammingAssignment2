## This function creates a cached inverted square matrix that can be retrieved
## outside of the environment in which it was created
## the solve() function is used to compute the inverse of a squared matrix
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the inverse
        setInverse <- function(solve) m <<- solve
        
        # get the value of the inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

#================================================================================

## This function returns a squared matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        
        # check to see if the matrix has already been created
        # if already created, pulls the matrix from cached
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if matrix not created, retrieves the matrix and computes its inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
}

