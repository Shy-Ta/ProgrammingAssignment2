## a<-makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(a)
## Success, returning cached data
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## set/get value of matrix as well as inverse matrix in environment
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## attempt to retrieve from cache first (getinverse)
## find inverse matrix if not present in the cache and then add it to the cache for subsequent retrievals

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Success, returning cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
