## The following two functions are used to produce the inverse of a matrix which had been stored in the cache
## If the matrix already exist it reproduces the previous result, if not the inverse is calculated

## makeCacheMatrix sets the relevant matrix, gets the matrix, set the inverse of the relevant matrix and set the inverse of the relevant matrix

makeCacheMatrix <- function(x = matrix()) {
        matrix.inv <- NULL
        set <- function(y) {
                x <<- y
                matrix.inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrix.inv <<- inverse
        getInverse <- function() matrix.inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolves fetches a previously calculated cached inverse of a matrix if available, if not, the inverse is calculated of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix.inv <- x$getInverse()
        if (!is.null(matrix.inv)) {
                message("getting cached data")
                return(matrix.inv)
        }
        mat <- x$get()
        matrix.inv <- solve(mat, ...)
        x$setInverse(matrix.inv)
        matrix.inv
}
