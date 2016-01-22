## We want to calculate the inverse of a matrix
## To make the computation faster, we will first cache the value of the inverse ('invs') so that it needs not be recalculated every time we need it. It will instead be retrieved from the cache
## A first function will create a special "matrix" object that can cache its inverse
## A second function will calculate the inverse of the matrix returned by the first function, but if the inverse already exists, it will retrieve it from the cache

## makeCacheMatrix() takes input and turns it into a matrix
## It defines four functions and puts them into a list
## invs creates a local version of the object that will be inverse of the matrix, assigning it to 'NULL'
##  1. set the value of the matrix and its inverse in another environment. So if you want to change the value of the matrix, provide some input
##  2. get the value of the matrix 
##  3. set the value of the inverse in another environment. So if you want to change the value of the inverse (now it is NULL), provide some input
##  4. get the value of the inverse

## makeCacheMatrix() returns a list with four elements, which are the four functions defined within it

makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL     
    set <- function(y) { 
            x <<- y
            invs <<- NULL
    }
	get <- function() {
	    x
	}
	setinverse <- function(inverse) {
	    invs <<- inverse
	}
	getinverse <- function() {
	    invs
	}
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() calculates the inverse of the matrix created with makeCacheMatrix().
## - In the beginning, it checks whether the inverse has already been calculated. (> if(!is.null(invs))) 
## - If so, it gets the inverse from the cache and returns it exiting the function ('return')
## - If there is no inverse in the cache, it calculates it sets its value in the cache using setinverse(), which was define within makeCacheMatrix(), in another environment

## NOTE: a non-square matrix never has an inverse, and square matrices whose determinant is equal to 0 are also non-invertible
## So an error message will be thrown, like:
## "Error in solve.default(data, ...) : 'a' (3 x 1) must be square"
## "Error in solve.default(data, ...) : 
##  Lapack routine dgesv: system is exactly singular: U[2,2] = 0"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinverse(invs)
        invs       
}
