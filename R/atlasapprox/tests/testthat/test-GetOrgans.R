# File: tests/testthat/test-GetOrgans.R

test_that("GetOrgans returns correct organs for various species", {
  
  # List of species that has Whole organ
  species_with_whole_organ <- c(
    "a_queenslandica", "c_elegans", "c_gigas", "c_hemisphaerica", "d_rerio",
    "h_miamia", "i_pulchra", "l_minuta", "m_leidyi", "n_vectensis",
    "p_crozieri", "p_dumerilii", "s_lacustris", "s_mansoni", "s_mediterranea", 
    "s_pistillata", "s_purpuratus", "t_adhaerens", "t_aestivum"
  )
  
  # Test each species that should return only "whole"
  for (species in species_with_whole_organ) {
    result <- GetOrgans(species)
    message("Testing organs for species: ", species)
    
    expect_equal(length(result), 1, 
                 info = paste("Expected length 1 for", species, "but got", length(result)))
    
    # Use expect_identical instead of expect_equal
    expect_identical(as.character(result), "whole", 
                     info = paste("Expected 'whole' for", species, "but got", paste(result, collapse = ", ")))
  }
  
  # Some edge cases
  expect_identical(as.character(GetOrgans("f_vesca")), "leaf")
  expect_identical(as.character(GetOrgans("o_sativa")), "root")
  # instead of this, check if seedling is one of the return.
  expect_setequal(as.character(GetOrgans("z_mays")), "seedling")
  
  
  # Test a few species that should return multiple organs
  species_with_multi_organ <- list(
    "h_sapiens" = c("bladder", "blood", "colon", "eye", "fat", "gut", "heart", "kidney", "liver", "lung", "lymphnode",
                    "mammary", "marrow", "muscle", "pancreas", "prostate", "salivary", "skin", "spleen", "thymus",
                    "tongue", "trachea", "uterus"),
    "m_musculus" = c("bladder", "colon", "fat", "heart", "kidney", "liver", "lung", "mammary", "marrow", "muscle",
                     "pancreas", "skin", "spleen", "thymus", "tongue", "trachea")
  )
  
  for (species in names(species_with_multi_organ)) {
    result <- GetOrgans(species)
    expected_organs <- species_with_multi_organ[[species]]
    
    # Ensure that the result matches the expected organs
    expect_true(all(expected_organs %in% result), 
      info = paste("Expected organs for", species, "but missing:", 
      paste(setdiff(expected_organs, result), collapse = ", ")))
  }
  
})
