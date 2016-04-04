## Fonction pour créer une matrice dont l'inverse est mis en cache
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL   
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinv <- function(inv) m <<- inv
       getinv <- function() m
       list(set = set, get = get,
            setinvn = setinv,
            getinv = getinv)
       
}

## Fonction pour calculer l'inverse s'il ne l'est pas déjà, ou pour le
## rappeler s'il se trouve en cache

cacheSolve <- function(x, ...) {
       m <- x$getinv()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinv(m)
       m
}
