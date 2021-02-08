#Murph
#Coursera Week3
#The following function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setiv <- function(solve) m <<- solve
        getiv <- function() m
        list(set = set, get = get,
             setiv = setiv,
             getiv = getiv)
}
#The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        m <- x$getiv()
        if(!is.null(m)) {
                message("TO cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setiv(m)
        m
}
