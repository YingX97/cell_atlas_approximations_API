library(testthat)
library(atlasapprox)

# Use fixtures to improve the test

# Shared Variables: Store commonly used data in a fixture.
setup({
  test_organism <<- "h_sapiens"
  test_tissue <<- "lung"
  test_gene <<- "COL1A1"
  test_genes <<- c("COL1A1", "PTPRC")
  test_celltype <<- "fibroblast"
  test_number <<- 10
})

# Clean up the test environment if needed
teardown({
  rm(test_organism, test_tissue, test_genes, test_celltype, envir = .GlobalEnv)
})

# Test cases using the shared fixtures

test_that("GetOrganisms works", {
  organisms <- GetOrganisms()
  expect_false(is.null(organisms))
  expect_type(organisms, "character")
  expect_gte(length(organisms), 28)
})

test_that("GetOrgans works", {
  organs <- GetOrgans(test_organism)
  expect_type(organs, "character")
  expect_true(length(organs) > 6)
})

test_that("GetCelltypes works", {
  celltypes <- GetCelltypes(test_organism, test_tissue)
  expect_type(celltypes, "character")
  expect_true(length(celltypes) > 6)
})

test_that("GetAverage works", {
  result_avg <- GetAverage(test_organism, test_tissue, test_genes)
  expect_true(is.list(result_avg))
  expect_equal(length(result_avg), length(test_genes))
  expect_equal(tolower(names(result_avg)), tolower(test_genes))
})

test_that("GetFractionDetected works", {
  result_frac <- GetFractionDetected(test_organism, test_tissue, test_genes)
  expect_true(is.list(result_frac))
  expect_equal(length(result_frac), length(test_genes))
  expect_equal(tolower(names(result_frac)), tolower(test_genes))
})

test_that("GetMarkers works", {
  markers <- GetMarkers(test_organism, test_tissue, test_celltype, 5)
  expect_type(markers, "character")
  expect_equal(length(markers), 5)
})

test_that("GetCelltypeLocation works", {
  locations <- GetCelltypeLocation(test_organism, test_celltype)
  expect_type(locations, "character")
  expect_true(length(locations) > 1)
})

test_that("GetSimilarFeatures works", {
  similar_features = GetSimilarFeatures(test_organism, test_tissue, test_gene, test_number, "correlation")
  expect_true(nrow(similar_features) == test_number, paste("Should have", test_number, "rows"))
  expect_true(ncol(similar_features) >= 2, "Should have at least 2 columns: features and distance")
})

test_that("GetDataSources works", {

  datasources <- GetDataSources()
  expect_false(is.null(datasources))
  expect_type(datasources, "list")
  expect_true(length(datasources) > 0)

  # Check for specific expected keys in the list
  expected_keys <- c("a_queenslandica", "c_elegans", "h_sapiens", "z_mays")
  for (key in expected_keys) {
    expect_true(key %in% names(datasources), 
      info = paste("Expected key", key, "not found in datasources"))
  }

})