## The following two functions help avoid repeated computation of inverse of a
## matrix by storing the inverse in a cache. This way the inverse is computed
## just once when it is needed for the frst time. For subsequent uses, the 
## previous computed and cached inverse is used

## makeCacheMatrix function takes a matrix as an argument. It stores this 
## matrix in the cache, initializes its inverse to NULL and stores that also
## in cache. It then returns a list of functions (get, set, getInv and setInv)
## to get the matrix, set the matrix, get its inverse and set its inverse
## respectively

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invM <<- inv
    getInv <- function () invM
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve function takes the list of functions created by 
## makeCacheMatrix function and returns the inverse of the matrix 
## stored in the cache. 
## At the first invocation, the function actually computes the inverse
## and stores that also in the cache. At subsequent invocation, the 
## simply returns the stored inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## This function does not really take the matrix as input parameter but the
    ## special list that we create using makeCacheMatrix function. I asseme that
    ## is what the assignment is supposed to do. Because if we send just
    ## the matrix as a parameter, we cannot make use of cache
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
