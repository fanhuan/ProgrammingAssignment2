## Below are two functions that used to creat a special object
## that stores a matrix and caches its inverse.

## The first function creates a special vector which is a list
## containing a function to set/get the value of the matrix and
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function returns the inverse of a matrix while checking
## whether its inverse has been calculated before or not.

cacheSolve <- function(x) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
