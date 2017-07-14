# The two function below are used to create a
# special object that stores the matrix
# and cache its inverse.

# The makeCacheMatrix function creates a special matrix 
# that can cache its inverse.  The special "matrix" 
# is a list containing a function to:
# set the value of the matrix, get the value of matrix,
# set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
        x<<-y
        m<<-NULL
        }
        get<-function()x
        setinverse<-function(solve) m<<-solve
        getinverse<-function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


# The cacheSolve function first checks in the cache
# if the inverse of the matrix has
# already been computed (and the matrix has not
# changed)and if so gets the inverse from the cache.
# If not, it computes the inverse
# of the special matrix and sets the inverse of the matrix
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
        

