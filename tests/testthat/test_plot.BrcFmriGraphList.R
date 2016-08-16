context("Test plot.BrcFmriGraphList")


## test .findUniqueParcels

test_that("it finds the right parcels", {
  mat <- matrix(c(rep(0,15),1:10),5,5)
  expect_true(all(c(.findUniqueParcels(mat) == c(1:10))))
})

test_that("it works when the matrix is all 0", {
  mat <- matrix(rep(0,25),5,5)
  expect_true(length(.findUniqueParcels(mat)) == 0)
})


# Test .isColor()

test_that("it returns TRUE if the string is a valid color, and FALSE if not", {
  actual <- .isColor(c("asdf", "#000000"))
  expected <- c(FALSE, TRUE)
  expect_equal(actual, expected)
})

# test .defaultColors()

test_that("the first color is black", {
  colors <- .defaultColors(5)
  expect_equal(colors[[1]], "#000000FF")
})

test_that("it gives you one color for each parcel plus black", {
  colors <- .defaultColors(5)
  expect_equal(length(colors), 6)
})

# Test .parcellationToArray()

dim3d <- c(2, 2, 2)
partition <- c(0, 0, 1, 1, 2, 2, 3, 3)
parcel <- brcbase::BrcParcellation(dim3d=dim3d, partition=partition)

test_that("the dims of the array should be the dims of the parcellation", {
  arr <- .parcellationToArray(parcel)
  expect_equal(dim(arr), c(2, 2, 2))
})


# test .makeIndexSequence()

test_that("it should return a sequence of the user-specified length", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_equal(length(seq), 13)
})

test_that("it should return a vector of integers", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_equal(seq, as.integer(seq))
})

test_that("all of the values should be positive", {
  seq <- .makeIndexSequence(max=15, length=13)
  expect_true(all(seq > 0))
})

# test .extractSlices()
arr <- array(c(rep.int(0, 16),
               0, 0, 0, 0, 0, 1, 2, 0, 0, 3, 4, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 5, 6, 0, 0, 7, 8, 0, 0, 0, 0, 0,
               rep.int(0, 16)),
               dim=c(4, 4, 4))

test_that("it should return a list of the same length as the indices list", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=1)
  expect_equal(length(slices), 2)
})

test_that("it should contain elements whose dimensions are one less than arr", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=1)
  dimensionLengths <- lapply(slices, function(x) length(dim(x)))
  expect_true(all(dimensionLengths == length(dim(arr)) - 1))
})

test_that("it contains the slices corresponding to the indices", {
  slices <- .extractSlices(arr=arr, indices=c(2, 3), dim=2)
  expect_equal(slices, .splitAlongDim(arr, 2)[2:3])
})

# test .plotLayout()

test_that("the product of nrow and ncol is larger than numSlices", {
  layout <- .plotLayout(10)
  expect_true(layout$nrow * layout$ncol >= 10)
})

# test splitAlongDim()

arr <- array(c(1, 2, 3, 4, 5, 6, 7, 8), dim=c(2, 2, 2))

test_that("it splits an array along a dimension", {
  expected <- list(array(c(1, 2, 3, 4), dim=c(2, 2)),
                   array(c(5, 6, 7, 8), dim=c(2, 2)))
  expect_equal(.splitAlongDim(arr, 3), expected)
})

test_that("you can choose the dimension along which to split", {
  expected <- list(array(c(1, 2, 5, 6), dim=c(2, 2)),
                   array(c(3, 4, 7, 8), dim=c(2, 2)))
  expect_equal(.splitAlongDim(arr, 2), expected)
})