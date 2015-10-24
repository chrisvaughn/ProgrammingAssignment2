## set of functions to allow for computing the inverse of
#  of a matrix and caching the result to reduce computing time

############################
# example usage
# build matrix as usual
vec <- c(1, 1, 1, 1, 1,
         0, 1, 0, 0, 0,
         0, 0, 1, 0, 0,
         0, 0, 0, 1, 0,
         0, 0, 0, 0, 1)
mat <- matrix(vec, ncol=5, byrow=TRUE)
# build a cacheMatrix object
cachableMat <- makeCacheMatrix(mat)
# first iteration of the loop will compute
# and cache the resuly next 4 will return
for (i in 1:5) {
    cacheSolve(cachableMat)
}
############################

## func to build a cacheMatrix "object" that will
#  allow for setting & getting the original
#  matrix and the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    # variable to hold cached inverse result
    m <- NULL
    # func to set matrix and clear cached result
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # func to get matrix
    get <- function() x
    # func to set inverse
    setinverse <- function(inverse) m <<- inverse
    # func to get inverse
    getinverse <- function() m
    # build "object"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## func that uses a CacheMatrix "object" to
#  compute and cache inverse. This func will
#  compute the inverse only if this is the first
#  time this method was called on a CacheMatrix
# "object"
cacheSolve <- function(x, ...) {
    # try to get the inverse from the CacheMatrix
    # and test to see if it was set
    m <- x$getinverse()
    if(!is.null(m)) {
        # if the inverse was set then just return it from
        # cache
        message("getting cached data")
        return(m)
    }
    # inverse was not cached so get the original matrix
    # compute the inverse, cache the inverse, and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
