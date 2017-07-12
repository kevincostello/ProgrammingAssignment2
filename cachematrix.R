## Function makeCacheMatrix creates an R object that stores a matrix and its inverse 
## Function cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
## or returns the cached value

## This function sets up a list which does the following

# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of the inverse of the matrix
# 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
        
}

aMatrix <- makeCacheMatrix(matrix(c(1:9), nrow = 3, ncol = 3)) # intialise matrix

aMatrix$get()               # retrieve the value of x
aMatrix$getmatrix()           # retrieve the value of m, which should be NULL

# reset value with a new matrix, create 3 x 3 matrix filled with a random sample of numbers between 1 and 9
aMatrix$set(matrix(c(2,3,6,1,3,4,6,8,1), nrow = 3, ncol = 3)) # reset value with a new matrix
aMatrix$get()               # retrieve the value of x
cacheSolve(aMatrix)          # notice inverse of the matrix calculated is of new matrix, not old matrix
aMatrix$getmatrix()           # retrieve it directly, now that it has been cached
cacheSolve(aMatrix)          # Running the function cacheSolve again will retrieve the cached value

# Prove result from cacheSolve function matches running solve function directly on same matrix
solve(matrix(c(2,3,6,1,3,4,6,8,1), nrow = 3, ncol = 3)) == cacheSolve(aMatrix)
