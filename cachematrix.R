## These two functions combine to calculate and cache inverses of matrices. A check will
## be made to see if the cache contains an already-caculated inverse. If it does, it is
## returned. Otherwise, the invers is calculated and stored in the cache.

## a function which creates a vector that containst a list of functions
## used to cache its own inverse
makeCacheMatrix <-  function(x = matrix()) {
                    m <- NULL
                    
                    ## this sets the matrix
                    set <- function(y) {
                        x <<- y
                        m <<- NULL
                    }
                    ## this gets the matrix
                    get <- function() x
                    
                    ## functions to set/get the inverse of the matrix
                    setInverse <- function(inverse) m <<- inverse
                    getInverse <- function() m
                    list(set = set, get = get,
                         setInverse = setInverse,
                         getInverse = getInverse)
}

## This function checks to see if the inverse of the matrix has already been cached.
## If it has, it retrieves it. Otherwise, it computes the inverse and caches it
cacheSolve <-   function(x, ...) {
    
                    ## Return a matrix that is the inverse of 'x'
                    m <- x$getInverse()
                    
                    ## check to see if the inverted matrix is already cached
                    ## if it is, retrieve it instead of calculating it again
                    if(!is.null(m)) {
                        message("retrieving cached data...")
                        return(m)
                    }
                    
                    ## calculate the inverse and cache it (since it wasn't already)
                    timerStart2 <- Sys.time()
                    data <- x$get()
                    m <- solve(data, ...)
                    x$setInverse(m)
                    print(timerStop2 <- Sys.time() - timerStart2) ## how long did it take?
                    m
}
