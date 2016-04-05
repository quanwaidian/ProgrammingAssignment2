makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse property
    a<-NULL
    ## Set the matrix
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    ## Get the matrix
    get<-function() {
        x
    }
    ## Inverse
    setsolve<-function(solve) a<<- solve
    getsolve<-function() {
        a
    }
    ## Return
    list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x=matrix(), ...) {
    ## return inverse matrix
    a<-x$getsolve()
    ## return already exist matrix
    if(!is.null(a)){
        message("getting cached data")
        return(a)
    }
    matrix<-x$get
    a<-solve(matrix, ...)
    ## inverse
    x$setsolve(a)
    a
}
