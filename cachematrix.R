## These two function (MakeCacheMatrix and cacheSolve) find the inverse of a 
## matrix and uses lexical scoping to store the results in a cache.  If the
## inverse of matrix that was already calculated is requested again, the function
## returns the version stored in cache.  Since matrix inversion is an expensive
## operation (in terms of compute), caching dramatically improves response time.

## The makeCacheMatrix function accepts an ordinary matrix and stores it as a
## container in the functions environment.  This will allow previously
## calculated inverses to be re-called from this 'cache' rather than re-calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvs <- function(solve) m <<- solve
        getInvs <- function() m
        list(set = set, get = get,
             setInvs = setInvs,
             getInvs = getInvs)

}


## The cacheSolve function accepts a matrix as input then checks the cache to 
## see if the inverse has already been calculated.  If it has, the functions
## prints a message to alert the user that the results are being re-called.
## If the matrix hasn't already been calculated, this function calculates the 
## inverse using R's 'solve' function and stores the result in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvs()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvs(m)
        m
}
