## These functions cache the inverse of a matrix.
## Assumption: the matrix supplied is a square invertible matrix


## makeCacheMatrix creates a special "matrix" object (x) that can cache 
## its inverse.
## In specific, makeCacheMatrix creates a list containing a function to
##
## set the value of the matrix (and clear cached inverse of matrix)
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- matrix()
    
    set <- function(y) {
        x <<- y #assumes y is a square invertible matrix
        inv <<- matrix()
    }
    
    get <- function() x
    
    setInverse <- function(i) inv <<- i
    
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated
## (and the matrix has not changed), then this function returns
## the inverse from the cache.
## Otherwise, this function calculates the inverse, caches it, and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        # Check if inv is empty
        if(length(inv) == 1 && is.na(inv[1])){
            ##calculate inverse, cache it and return it
            data <- x$get()
            inv <- solve(data)
            x$setInverse(inv)
            return(inv)
        }
        
        # otherwise, return matrix that is already cached 
        message("getting cached data")
        return(inv)
        
}


