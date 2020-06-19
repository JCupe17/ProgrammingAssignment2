## These functions compute the inverse of a matrix
## and return the value if cached / computed before

## This function creates a special vector
## which contains a list containing a function
## to set the value of the vector, get the value
## of the vector, set the value of the inverse of matrix, and
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inv <<- inverse
    getinverse <- function() m_inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the cachec value for the
## inverse of a matrix if not it computes it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inv <- x$getinverse()
    if(!is.null(m_inv)) {
        message("getting cached inverse of matrix")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinverse(m_inv)
    m_inv
}
