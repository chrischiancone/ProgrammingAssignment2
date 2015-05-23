## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        #setmean <- function(mean) m <<- mean
        s_inv <-  function(v_in) mi <<- v_in 
        g_inv <-  function() mi
        list(set=set, get=get, s_inv=s_inv, g_inv=g_inv)
}


## Write a short comment describing this function


cacheSolve <- function(c_x, ...) {
        mi <- c_x$g_inv()
        if (!is.null(mi)){
                message("getting data")
                return(mi)
        }
        mat.data <- x$get()
        mi <- solve(mat.data, ...)
        x$s_inv(mi)
        return(mi)
}

