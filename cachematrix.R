makeCacheMatrix <- function(x = matrix()) {
    a<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setsolve<-function(solve) a<<- solve
    getsolve<-function() a
    list(set=set, get=get,setsolve=setsolve,getsolve=getsolve)
}

cacheSolve <- function(x=matrix(), ...) {
    a<-x$getsolve()
    if(!is.null(a)){
        message("getting cached data")
        return(a)
    }
    matrix<-x$get
    a<-solve(matrix, ...)
    x$setsolve(a)
    a
}
