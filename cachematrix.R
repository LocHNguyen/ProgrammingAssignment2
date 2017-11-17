##This is a pair of functions that can cache the inverse of a matrix.

## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function( x = matrix () )
{       
        inverse = NULL
        set = function(y)
        {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(solvematrix) inverse <<- solvematrix
        get_inverse <- function() inverse
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


##This function computes the inverse of the special matrix that makeCacheMatrix calculated.

cacheSolve <- function(x, ...)
{
        inverse = x$get_inverse()
        if(!is.null (inverse))          ##If makeCacheMatrix has not be changed, output result.
        {
                message("getting cache data")
                return(inverse)
        }
        data = x$get()                  ##If makeCacheMatrix has been changed, recalculated and return results.
        inverse = solve(data, ...)
        x$set_inverse(inverse)
        return(inverse)
}
