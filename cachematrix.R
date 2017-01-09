## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
			z<-NULL
			set<function(y){
				x<<-y
				z<<-NULL
			}
			get<-function() x
			setInv<-function(inverse) z<<-inverse
			getInv<-function() z
			list(set=set, get=get,
				 setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		z<-x$getInv()
		if(!is.null(z)) {
			message("getting cashed data")
			return(z)
		}
		m<-x$get()
		z<-inverse(m,...)
		x$setInv(z)
		z
}
