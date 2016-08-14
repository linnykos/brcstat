context("Test estimate.Graph")

mat <- matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4)
partition <- c(0, 1, 2, 2, 3, 3, 4, 0)
parcellation <- brcbase::BrcParcellation(dim3d=c(2, 2, 2), partition)
mri <- brcbase::BrcFmri(data2d=mat, id="01", parcellation=parcellation)
rm(list = c("mat", "partition", "parcellation"))

test_that("it produces a valid BrcFmriGraphList obj", {
  obj <- estimateGraph(mri)
  expect_true(brcbase::isValid(obj))
})