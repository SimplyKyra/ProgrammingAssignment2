## Steps to use these methods:
##  1. Create a matrix (ie. x)
##  2. Call makeCacheMatrix, while passing in x (step 1) and save result:
##          y <- makeCacheMatrix(x)
##  3. Call cacheSolve and pass in y (step 2).

## Creates a special 'matrix' which is really a list containing a function to 
##  1. Set the value of the matrix
##  2. Get the value of the matrix
##  3. Set the value of the inverse of the matrix
##  4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseOfMatrix <- function(inverseOfMatrix) inverseMatrix <<- inverseOfMatrix
    getInverseOfMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseOfMatrix = setInverseOfMatrix,
         getInverseOfMatrix = getInverseOfMatrix)
}


## Checks to see if the inverse of the matrix has already been calculated. If so
## it gets the inverse of the matrix from the cache and skips the computation;
## otherise, it calculates the inverse of the matrix and sets the value in the 
## cache via the setInverseOfMatrix funtion.

cacheSolve <- function(x, ...) {
    m <- x$getInverseOfMatrix()
    if (!is.null(m)) {
        message("Getting the cached data.")
        return(m)
    }
    ## Calculating the inverse.
    data <- x$get()
    m <- solve(data)
    x$setInverseOfMatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
