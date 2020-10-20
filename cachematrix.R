## Put comments here that give an overall description of what your
## functions do


## Function makeCacheMatrix:
makeCacheMatrix <- function(M = matrix()) {
        I <- NULL
        setM <- function(S) {
                M <<- S
                I <<- NULL
        }
        getM <- function() M
        setMinv <- function(solve) I <<- solve
        getMinv <- function() I
        list(setM = setM, getM = getM, setMinv = setMinv, getMinv = getMinv)
}


## Function cacheSolve:
cacheSolve <- function(M, ...) {
        x <- M$getMinv()
        if(!is.null(x)) {
                message("getting cached data")
                return(x)
        }
        y <- M$getM()
        x <- solve(y, ...)
        M$setMinv(x)
        x
}
