## The following two functions compute and cache the inverse of a matrix under
## two scenarios. 1) If the inverse of a matrix has not been computed, the
## inverse will be computed and cached. 2) If the inverse of a matrix 
## has been computed, the inverse can be looked up in the cache and returned 
## rather than being recomputed.

## The fucntion makeCacheMatrix takes an invertible numeric matrix as input and 
## returns a sepecial "matrix", which is really a list containing a function to: 
## 1) set the value of the matrix 
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" object
## returned by makeCacheMatrix above. If the inverse has been computed, it 
## retrieves the inverse from the cache and skips the computation. Otherwise, 
## it computes the inverse of the matrix and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
