# My functions read from the input a matrix reversible and return the 
# reverse matrix.
# Introduce the <<- operator which can be used to assign a value to an 
# object in an environment that is different from the current environment.
# Below are two functions that are used to create a special object that 
# stores a numeric matrix and cache's its reverse.

# The first function, makeCacheMatrix creates a  "matrix", which 
# containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

#
cachematrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function (y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function calculates the reverse of the "matrix" created 
# with the above function. However, it first checks to see if the 
# inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via 
# the setInverse function.

cacheSolve <- function(x, ...) 
{
        inv <- x$getInverse()
        if (!is.null(inv))
        {
                message("obteniendo datos del caché")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
## Return a matrix that is the inverse of 'x'
