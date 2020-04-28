
makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <-function(matrix){
    x<<-matix
    m<<-NULL
  }
  get<- function(){
    x
  }
  setInverse <- function(inverse) {
    m <<- inverse
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}



