## Caching the Inverse of a Matrix
## 缓存逆矩阵

## Creates a special “matrix” object that can cache its inverse
## 创建可缓存逆矩阵的特殊“矩阵”对象

makeCacheMatrix <- function(x = matrix()) {
    ivs <- NULL
    set <- function(y) {
        x <<- y
        ivs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ivs <<- inverse
    getinverse <- function() ivs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## Computes the inverse of the special “matrix” returned by makeCacheMatrix
## 计算makeCacheMatrix函数返回的特殊“矩阵”的逆矩阵
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
## 如果已经计算过逆矩阵（且矩阵没有发生更改）
## 那么cachesolve函数将检索缓存中的逆矩阵。

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## 返回矩阵“x"的逆矩阵
    ivs <- x$getinverse()
    if(!is.null(ivs)) {
        message("getting cached data")
        return(ivs)
    }
    data <- x$get()
    ivs <- solve(data, ...)
    x$setinverse(ivs)
    ivs
    
}
