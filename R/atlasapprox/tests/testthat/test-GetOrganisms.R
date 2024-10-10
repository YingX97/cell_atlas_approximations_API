# Start the test block for GetOrganisms
test_that("GetOrganisms returns expected output", {
  
  # Run the GetOrganisms function
  organisms <- GetOrganisms()
  
  # Check that the result is not null
  expect_false(is.null(organisms))
  
  # Check that it returns a vector (array)
  expect_type(organisms, "character")
  
  # Check that it contains at least 28 species, gte(greater than expect)
  expect_gte(length(organisms), 28)
  
  expected_organisms <- c(
    "a_queenslandica", "a_thaliana", "c_elegans", "c_gigas", "c_hemisphaerica",
    "d_melanogaster", "d_rerio", "f_vesca", "h_miamia", "h_sapiens",
    "i_pulchra", "l_minuta", "m_leidyi", "m_murinus", "m_musculus",
    "n_vectensis", "o_sativa", "p_crozieri", "p_dumerilii", "s_lacustris",
    "s_mansoni", "s_mediterranea", "s_pistillata", "s_purpuratus", "t_adhaerens",
    "t_aestivum", "x_laevis", "z_mays"
  )
  
  # Check if all expected organisms are present
  missing_organisms <- setdiff(expected_organisms, organisms)
  expect_true(length(missing_organisms) == 0, info = paste("Missing organisms:", paste(missing_organisms, collapse = ", ")))
  
  # Check if there are no unexpected organisms
  unexpected_organisms <- setdiff(organisms, expected_organisms)
  expect_true(length(unexpected_organisms) == 0, info = paste("Unexpected organisms:", paste(unexpected_organisms, collapse = ", ")))
  
})