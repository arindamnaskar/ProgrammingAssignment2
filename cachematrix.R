## The following functions determines the inverse of a matrix 
## and cache the value for future use


## makeCacheMatrix returns a list with the following functions:
## set : sets the value of the matrix in a different environement
## get : gets the value of the matrix
## setInverse : sets the value of the inverse matrix in a different 
## environement than the current environement(caching)
## getInverse : gets the value of the inverse matrix(from cache)

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <<- NULL
    set <- function(y){
        x <<- y
        mat_inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) mat_inv <<- inv 
    getInverse <- function() mat_inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function returns the inverse of a matrix if it is already present 
## in the cache. Otherwise it calculates the inverse and caches it and returns
## the value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
       message("getting cached data")
       return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
