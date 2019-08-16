## makecacheMatrix - set matrix, get matrix, set Matrix Inverse and get Matrix Inverse
## CacheSolve - Will get the Inverse of Matrix calculated and cached by makeCacheMatrix Function

## Will take a matrix from user. Where not available will take a default 2x2 matrix of ones

makeCacheMatrix <- function(x = matrix(numeric(1),2,2)) {
    xInv <- NULL
    setx <- function(y){
        x <<- y
        xinv <<- NULL
    }
    getx <- function() x
    setInv <- function(xInv){
        xInv <<- xInv
    }
    getInv <- function() xInv
    list(setx = setx, getx = getx, setInv = setInv, getInv = getInv)
}


## CacheSolve Will retrieve the Inverse calculated and cached by MakeCacheMatrix function.
## Where no cache found, will solve the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if (!is.null(xInv)){
        xInv1 <- solve(xInv)
        if(x == xInv1) {
            message("getting cached data")
            return(xInv)
        }
    }
    data <- x$getx()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}
