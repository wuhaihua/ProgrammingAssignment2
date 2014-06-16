## Put comments here that give an overall description of what your
## functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL  ## init the inverse matrix to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL  ## clear inverse matrix if original matrix changes
        }
        get <- function() x
        setinverse <- function(inversematrix) inverse <<- inversematrix
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if (!is.null(inverse)) {  ## inverse matrix cache hit
                message("getting cached data")
                return(inverse)
        }
		## cache miss, need generate inverse matrix
        data <- x$get()
        inverse <- solve(data, ...)  ## generate inverse matrix, assume original matrix is inversible
        x$setinverse(inverse)
        inverse  ## return the inverse matrix
}
