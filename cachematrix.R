#make cache matrix
makeCacheMatrix <- function(x = matrix()) {
  #initialize 
  m<- NULL
  #set the matrix
  set <-function(matrix){
    x<<-matix
    m<<-NULL
  }
  #get the matrix
  get<- function(){
    x
  }
  #inverse the matrix
  setInverse <- function(inverse) {
    m <<- inverse
  }
  #get inverse of matrix
  getInverse <- function() {
    m
  }
  #return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cachesolve

cacheSolve <- function(x, ...) {
  #return inverse matrix
  m <- x$getInverse()
  #return the inverse if it is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  #get matrix
  data <- x$get()
  #calculate inverse
  m <- solve(data,...)
  #set inverse of object
  x$setInverse(m)
  m
}



