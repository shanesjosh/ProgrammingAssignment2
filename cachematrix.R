## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invMat <- NULL
	setMat <- function (y) {
		x <<- y
		invMat <<- NULL
	}
	getMat <- function() x
	setInv <- function(inverse) invMat <<- inverse
	getInv <- function() invMat
	list(setMat = setMat, getMat = getMat, setInve = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	invMat <- x$getinv()
	if(!is.null(invMat)) {
		message("getting cached matrix")
		return(invMat)
	}
	matData <- x$getMat()
	invMat <- solve(matData, ...)
	x$setInv(invMat)
	invMat
}
