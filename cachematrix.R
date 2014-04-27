## What we want to do in this assigment is to create a couple of functions
## that check if the same calculation (inverse matrix) has been done before.
## If this is the case, the function returns the cached value, and if not,
## a new calculation is done. In the case of large calculations, that allows
## less time-consuming computations.

## The first function "makeCacheMatrix" creates a new type of object (a list in fact)
## that add the cache property. It takes as argument a "matrix" and returns
## a list with 4 functions (set, get, setsolve and getsolve). We can access to this
## functions through the operator "$" (i.e. "x$getsolve")

## Function "set" uses the operator "<<-". This operator is for setting a variable that
## already exists. In other words, it will keep going through the environments
## in order until it finds a variable with that name, and it will assign it to that.
## This can be within the scope of a function, or in the global environment.

## Function "get" returns the original matrix

## Function "setsolve" assgins the calculated inverse matrix to "m"

## Function "getsolve" returns the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function checks if a previous variable with the result we are looking for already exists.
## If the value of "m" if different from "NULL", that means we have calculated the inverse
## matrix previously, so we just return the value of "m".

## If "m" is "NULL", the function calculates the inverse matrix and returns its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
