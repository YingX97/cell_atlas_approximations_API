# File: tests/testthat/test-GetAverage.R

# Define test cases for different organisms, organs, and gene sets
test_cases <- list(
  list(organism = "h_sapiens", organ = "Lung", genes = c("COL1A1", "PTPRC")),
  list(organism = "m_musculus", organ = "Heart", genes = c("Actb", "Myh6")),
  list(organism = "d_rerio", organ = "Whole", genes = c("mmel1", "meis3", "foxi3a", "ca15a", "zgc:153760")),
  list(organism = "a_thaliana", organ = "Shoot", genes = c("AT1G01010", "AT1G01020","AT2G42840", "AT5G23940"))
)

# Start the test
test_that("GetAverage works across multiple species and organs", {
  
  for (case in test_cases) {
    # Extract organism, organ, and genes for this test case
    organism <- case$organism
    organ <- case$organ
    genes <- case$genes
    
    # Call the GetAverage function for this case
    result <- GetAverage(organism, organ, genes)
    
    # Print the case being tested
    message('Testing average expression of "', paste(genes, collapse = ", "), '" in ', organism, ' ', organ)
    
    # Check that the result is a list
    expect_true(is.list(result))
    
    # Test: Check the number of columns (matches the number of genes queried)
    expect_equal(length(result), length(genes))
    
    # Test: Check that the list names match the queried genes (case-insensitive)
    expect_equal(tolower(names(result)), tolower(genes))
    
  }
})
