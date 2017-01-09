## Functions caches the inverse of a matrix

## The function below creates a matrix object that cache its inverse

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


## The function below computes the inverse of the matrix returned by the makecashematrix function above.
## When it sees computed data, it will return the cashed data.

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
