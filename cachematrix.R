## Functions that cache the inverse of a matrix

## Caching the Inverse of a Matrix
## makeCacheMatrix is a function that returns a list of functions
makeCacheMatrix <- function( m = matrix() ) {

    ## Initialize the inverse property
    ## Initially nothing is cached so set it to NULL
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method to get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of the methods. Each named element of the list is a function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the inverse matrix
    m
}
