### Function makeCacheMatrix creates a list with data/methods to store matrix x;
### data includes the matrix x itself, was it inverted (flag m, inverted if m is not NULL), 
### and internal functions to operate the matrix: set, get, setInv, getInv. 
### If necessary, the function cacheSolve either calculates the inverse matrix (if it hasn't been calculated 
### already) and saves it, or returns the saved inverse matrix

### function makeCacheMatrix
### * Sets information about matrix x and flag m (if m is NULL it means matrix was not inverted);
### * Contains internal functions to operate the matrix: set, get, setInv, getInv. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(val) m <<- val
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


### Function cacheSolve
### Checks if flag m is null, then calculates the inverse matrix and saves it, 
### otherwise outputs already saved inverted matrix from m 
### and prints the message "getting cached data"
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
