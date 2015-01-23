## Allows the efficient calculation of a matrix inverse by solving for it once
## and storing for subsequent lookups. The makeCacheMatrix holds the actual 
## matrix and the cacheSolve function works with the makeCacheMatrix object to
## calculate the inverse and store it back into the makeCacheMatrix for future
## lookups

##
## Example
##
## create 2 x 2 matrix
## mat<-matrix(1:4, nrow=2,ncol=2)
##
## store it as a makeCacheMatrix object
## cachemat <- makeCacheMatrix(mat)
## 
## lookup the inverse
## cacheSolve(cachemat)
##
##       [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## subsequent lookups will use the cached inverse 
##

## 
## An object to store and retrieve a matrix and its inverse. 
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## 
## Returns the inverse of the matrix stored inside a makeCacheMatrix object. 
## For effeciencies only solves for the inverse once, storing the inverse
## in the makeCacheMatrix. Subsequent lookups use the cached inverse in
## makeCacheMatrix. 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
