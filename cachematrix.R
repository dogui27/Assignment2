
## Functions that:
##  - store an input matrix
##  - return matrix in input
##  - store inverse of matrix in input
##  - return inverse of matrix in input
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # check if new matrix is equal to previous one (ignoring names)
        mOld <- x; dimnames(mOld) <- NULL
        mNew <- y; dimnames(mNew) <- NULL
        if (!identical(1*mOld, 1*mNew)) {
            x <<- y
            inv <<- NULL
        }
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## Returns inverse matrix of x checking if it has been already calculated and if x is singular
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Inverse matrix already calculated -> retrieving cached data")
        return(inv)
    }
    data <- x$get()
    if (det(data) != 0) {
        inv <- solve(data, ...)
    }
    else {
        message("Matrix is singular -> no calculation")
        inv <- NULL
    }
    x$setinv(inv)
    return(inv)
}    

## checking functions
# a <- matrix(c(2, 4, 3, 1, 5, 7, 11, 3, 2), nrow = 3, ncol = 3, dimnames = list(c("x", "y", "z"), c("a", "b", "c")))
# m1 <- makeCacheMatrix(a)
# m1$get()
# m1$getinv()
# cacheSolve(m1)
# cacheSolve(m1)
# m1$set(matrix(c(2, 4, 3, 1, 5, 7, 11, 3, 2), nrow = 3, ncol = 3))
# cacheSolve(m1)
# m1$set(matrix(c(1, 4, 3, 1, 5, 7, 11, 3, 2), nrow = 3, ncol = 3))
# cacheSolve(m1)
# notInv <- matrix(1:9, nrow = 3, ncol = 3, dimnames = list(c("x", "y", "z"), c("a", "b", "c")))
# m1 <- makeCacheMatrix(notInv)
# cacheSolve(m1)
# cacheSolve(m1)
