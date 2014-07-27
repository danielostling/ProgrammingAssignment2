## danielostling: The functions makeCacheMatrix and cacheSolve provide
## functionality to lessen the computational burden of matrix inversion 
## operations.
##
## Initialize a cached matrix object using the makeCacheMatrix function, and 
## manipulate your matrix using the accessor functions provided. When the 
## inverse of the matrix object is needed, use the helper function cacheSolve. 
## Pass a CacheMatrix object to cacheSolve, and it will calculate the inverse 
## at first use. Later calls to cacheSolve will simply return the previously 
## computed inverse.

## danielostling: makeCacheMatrix
## Given a matrix, create a matrix object with extended functionality. Main 
## feature is caching of the inverse. Accessor functions:
## setMatrix - Set/store original matrix data.
## getMatrix - Get original matrix data.
## setInverse - Set/store inverse of original matrix.
## getInverse - Return inverse of original matrix (if available), else NULL.
makeCacheMatrix <- function(matrixInitial = matrix()) {
    # danielostling: Initialize the cached matrix inverse.
    matrixCachedInverse <- NULL

    # danielostling: Store the original matrix.
    setMatrix <- function(matrixInput) {
        matrixInitial <<- matrixInput
        matrixCachedInverse <<- NULL
    }

    # danielostling: Return the initial matrix.
    getMatrix <- function() {
        matrixInitial
    }

    # danielostling: Store the inverse of the original matrix.
    setInverse <- function(matrixInverse) {
        matrixCachedInverse <<- matrixInverse
    }

    # danielostling: Return the cached inverse of the original matrix.
    getInverse <- function() {
        matrixCachedInverse
    }

    # danielostling: Return a list of accessor functions for this object.
    list(setMatrix=setMatrix, getMatrix=getMatrix,
            setInverse=setInverse, getInverse=getInverse)
}


## danielostling: cacheSolve
## Given a CacheMatrix object, return its inverse. If the inverse is missing in 
## the CacheMatrix object, compute and store it in the CacheMatrix object 
## before returning the inverse. If the inverse is present in the CacheMatrix 
## object, return it directly to avoid additional computation.
cacheSolve <- function(matrixInput, ...) {

    # danielostling: Read cached inverse matrix.
    matrixInverse <- matrixInput$getInverse()
    
    # danielostling: The inverse matrix was present, return it.
    if(!is.null(matrixInverse)) {
        message("Returning inverse from cache.")
        return(matrixInverse)
    }
    
    # danielostling: The inverse was not present in the input matrix.
    # Calculate the matrix inverse, store it in input matrix, and return it to 
    # caller.
    matrixOrig <- matrixInput$getMatrix()
    matrixInverse <- solve(matrixOrig, ...)
    matrixInput$setInverse(matrixInverse)
    matrixInverse
}
