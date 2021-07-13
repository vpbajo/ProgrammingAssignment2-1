## Activity focus to exercise the formation of functions that either
## a matrix of a restore an inverse or cache

## makeCacheMatrix: is the function that sequence a matrix object to reserve data’s inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
        
        
}
        
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}



## cacheSolve: by the makeCacheMatrix is a role that authenticates the matrix’ inverse which was restored.
## In the matter of the inverse’s computation, from the cache the cacheSolve function will try to recover the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv}
