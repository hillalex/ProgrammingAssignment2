test_that("makeCacheMatrix can be initialised with a matrix", {
  m <- matrix(1:4,2,2)
  x <- makeCacheMatrix(m)
  expect_equal(x$get(), m)
})

test_that("makeCacheMatrix can be initialised without a matrix", {
  x <- makeCacheMatrix()
  expected <- matrix()
  expect_equal(x$get(), expected)
})

test_that("makeCacheMatrix object can set new matrix", {
  x <- makeCacheMatrix()
  m <- matrix(1:4,2,2)
  x$set(m)
  expect_equal(x$get(), m)
})

test_that("cacheSolve can get inverse of matrix", {
  m <- matrix(1:4,2,2)
  x <- makeCacheMatrix(m)
  i <- cacheSolve(x)
  expect_equal(m%*%i, diag(2))
})

test_that("cacheSolve can get inverse of matrix from cache", {
  m <- matrix(1:4,2,2)
  x <- makeCacheMatrix(m)

  # at this point the cached value is null
  expect_equal(x$getInverse(), NULL)

  expect_message(i <- cacheSolve(x), NA)

  # should have set cache now
  expect_equal(x$getInverse(), i)

  # and cacheSolve should use cache
  expect_message(cacheSolve(x), "getting cached inverse")
})
