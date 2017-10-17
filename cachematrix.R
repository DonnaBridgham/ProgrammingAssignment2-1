
## Creates a special "matrix" object that can cache its inverse.
##The function needs to be able to cache the inverse when called



makeCacheMatrix <- function(x = matrix()) {

        i  <- NULL

        set  <- function(y){

                x <<- y

                i <<- NULL 

        }

        get  <- function() x

        setinverse  <- function(inverse) i  <<- inverse

        getinverse  <- function() i

        list(set= set, get = get, 

             setinverse = setinverse, 

             getinverse = getinverse)



}

## setinverse and getinverse and not be interchanged



## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),

## then the cachesolve should retrieve the inverse from the cach



cacheSolve <- function(x, ...) {

        i  <- x$getinverse()

        if (!is.null(i)){

                message("getting cached data")

                return(i)

        }

        data  <- x$get()

        i  <- solve(data, ...)

        x$setinverse(i)

        i

}

##make sure to test functions from previous function
