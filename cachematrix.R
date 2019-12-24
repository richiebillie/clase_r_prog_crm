


makeCacheMatrix <- function(x = matrix()) { "DAMOS NOMBRE A LA FUNCION"
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

"OBTENEMOS LOS RESULTADOS DE LA MATRIZ"

x = rbind(c(3, -4), c(-8, 2)) 
m = makeCacheMatrix(x)
m$get()


