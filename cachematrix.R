## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        a<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) a<<- solve
        getmatrix<-function() a
        list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}




cacheSolve <- function(x, ...) {
        a<-x$getmatrix()
        if(!is.null(a)){
                message("getting cached data")
                return(a)
        }
        matrix<-x$get
        a<-solve(matrix, ...)
        x$setmatrix(a)
        a
}
