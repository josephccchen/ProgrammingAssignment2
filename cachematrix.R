## Put comments here that give an overall description of what your functions do
# Written for Coursera "R Programming" Week 3 Assignment

# makeCacheMatrix: create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  x <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# cacheSolve: compute the inverse of the special "matrix" returned by makeCacheMatrix above.
#             If the inverse has already been calculated, (and matrix has not changed), then:
#             cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}


