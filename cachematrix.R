## x as a matrix is the input that I have set
## ı wrote solved values that I denoted by s as NULL
## I also had to change the mean to solve

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
 s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The same logic of changing mean to solve applies also here.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
    if(!is.null(s)) {
        message("getting inversed matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

