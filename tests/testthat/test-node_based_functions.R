context("test-node_based_functions")
# these tests are a bit hacky

data(coquettes)

test_that("equal SR gives zero SOS", {
  repeats <- 1000
  method <- "rdtable"
  node_number <- 25
  
  results <- lapply(nodenumbers(coquettes), function(node) {
      ret <- nodiv_anal(node, coquettes, repeats, method)
      ret[,1:2]
  })
  SR <- sapply(results, "[[", 1)
  
  desc = Descendants(node_number, coquettes)
  desc1row = desc[1] - Nspecies(coquettes)
  desc2row = desc[2] - Nspecies(coquettes)
    
  SR[,desc1row] <- 1
  SR[,desc2row] <- 1
  
  sos <- parent_representation(node_number, SR, coquettes)
  expect_equal(sum(sos), 0)
})

test_that("valid p-values", {
  repeats <- 1000
  method <- "rdtable"
  
  results <- lapply(nodenumbers(coquettes), function(node) {
    ret <- nodiv_anal(node, coquettes, repeats, method)
    ret[,1:2]
  })
  rval <- sapply(results, "[[", 2)
  par_rval <- sapply(nodenumbers(coquettes), function(node) parent_representation(node, rval, coquettes))
  pval <- apply(par_rval, 2, pval_sig)
  not_na_pval <- pval[!is.na(pval)]
  
  expect_true(all(not_na_pval <= 1 & not_na_pval >= 0))
})
