## To solve a matrix once and store the inverse matrix for later retrieval,
## first pass a solvable matrix into makeCacheMatrix and store it in a variable.
## makeCacheMatrix takes in a solvable matrix and returns a list in which the 
## inverse matrix can later be stored by cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Then run cacheSolve on the variable. If cacheSolve has not been run on that
## variable previously, cacheSolve will solve the matrix and return the inverse.
## If cacheSolve has been run previously, it will return the previously stored
## inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
