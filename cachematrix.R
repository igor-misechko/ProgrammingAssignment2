## This functions creates a special "matrix" object and their inverse.
## The inverse after computation stored in cache and
## it's possible to read and use without new computations. 
## In addition getting 4 functions that make possible 
## to manipulate (set/read) the matrix and their inverse.

## This function creates a special "matrix" object 
## and 4 functions that make possible to manipulate the matrix
## ant their inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initialise the variable that will store the inverse matrix
    ## and assign their value to Zero in local environment.
    inv <- NULL
    
    ## We define function that we can give new matrix.
    set <- function(y) {
        ## Set value of 'x' in parent environment.
        x <<- y
        ## Set value of the inverse matrix in parent environment to Zero.
        ## Need to check condition if new matrix set.
        inv <<- NULL
    }
    
    ## We define function that we can simply return the value of 'x' in the current environment.
    get <- function() { x }
    ## We define function that return the inverse matrix to parent environment.
    setinverse <- function(inverse) { inv <<- inverse }
    ## We define function that return the inverse matrix to current environment.
    getinverse <- function() { inv }
    ## Make incuded functions as the output of makeCacheMatrix ()
    ## Its possible to use it in parent environment.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Assign to 'inv' value from parent environment.
    inv <- x$getinverse()
    ## If it's defined, 'inv' not NULL (computing before),
    ## then we return the cached value,
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## If 'inv' are NULL (we have new matrix), then computing inverse matrix.
    data <- x$get()
    inv <- solve(data, ...)
    ## Store the new value of inverse matrix in parent environment.
    x$setinverse(inv)
    
    inv
    
}
