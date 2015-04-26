## This is a pair of functions that cache the inverse of a matrix


## This function creates a special "matrix"object that can cache its
##inverse.

makeCacheMatrix<-function(ma=matrix()) {
        inverse<-NULL
        set<-function(x){
                ma<<-x
                inverse<<-NULL
                
        }
        get<-function() ma
        setinv<-function(inv) inverse<<-inv
        getinv<-function() inverse
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated(and the matrix has not changed),then the cachesolve should retrieve the inverse from the chache.

cacheSolve<-function(ma,...) {
        inverse<-ma$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data<-ma$get()
        inverse<-solve(data,...)
        ma$setinv(inverse)
        inverse
}
