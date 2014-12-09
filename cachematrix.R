## Caching the Inverse of a Matrix
## 缓存逆矩阵

## Creates a special “matrix” object that can cache its inverse
## 创建可缓存逆矩阵的特殊“矩阵”对象

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the ivs variable storing the inverse of a matrix
    ## 初始化存储逆矩阵的ivs变量
    ivs <- NULL
    ## Function to set the value of the matrix
    ## 设置矩阵值的函数
    set <- function(y) {
        x <<- y
        ivs <<- NULL
    }
    ## Function to get the value of the matrix
    ## 获取矩阵值的函数
    get <- function() x
    ## Function to set the value of the inverse
    ## 设置逆矩阵值的函数
    setinverse <- function(inverse) ivs <<- inverse
    ## Function to get the value of the inverse
    ## 获取逆矩阵值的函数
    getinverse <- function() ivs
    ## Create and return the list containing four functions
    ## 创建并返回含有上述四个函数的列表
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
    ## If the inverse has already been calculated, retrieve it from the cache
    ## 如果已经计算过逆矩阵，就从缓存中检索逆矩阵并返回它
    if(!is.null(ivs)) {
        message("getting cached data")
        return(ivs)
    }
    ## Otherwise, get the value of the matrix
    ## 否则，首先获取矩阵的值
    data <- x$get()
    ## Calculate the inverse of the matrix
    ## 然后计算矩阵的逆矩阵
    ivs <- solve(data, ...)
    ## Cache the inverse 
    ## 最后将计算出的逆矩阵进行缓存
    x$setinverse(ivs)
    ## Return the inverse
    ## 返回计算出的逆矩阵
    ivs
}
