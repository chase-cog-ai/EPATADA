# Test for TADA_GetATTAINSParamToWQPCharRef
test_that("TADA_GetATTAINSParamToWQPCharRef returns expected structure and data", {
  result <- TADA_GetATTAINSParamToWQPCharRef()
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 3)
  expect_true(all(c("CharacteristicName", "ATTAINS.ParameterName", "Alias.Type.Name") %in% colnames(result)))
  expect_true(nrow(result) > 0) # Ensure data is returned
})

test_that("TADA_GetATTAINSParamToWQPCharRef handles 'ATTAINS' argument correctly", {
  result <- TADA_GetATTAINSParamToWQPCharRef("ATTAINS")
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 3)
  expect_true(all(c("CharacteristicName", "ATTAINS.ParameterName", "Alias.Type.Name") %in% colnames(result)))
})

test_that("TADA_GetATTAINSParamToWQPCharRef handles invalid argument gracefully", {
  expect_error(TADA_GetATTAINSParamToWQPCharRef("InvalidArg"))
})

# Test for TADA_AdditionalCharAliasForReview
test_that("TADA_AdditionalCharAliasForReview works with default arguments", {
  result <- TADA_AdditionalCharAliasForReview()
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
})

test_that("TADA_AdditionalCharAliasForReview works with includeCST = TRUE", {
  result <- TADA_AdditionalCharAliasForReview(includeCST = TRUE)
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
})

test_that("TADA_AdditionalCharAliasForReview handles strictness correctly", {
  review_more_strict <- TADA_AdditionalCharAliasForReview(
    displayPercent = TRUE,
    ATTAINS.WQX.tolerance = 1.0,
    WQX.ATTAINS.tolerance = 1.0
  )
  expect_s3_class(review_more_strict, "data.frame")
  expect_true(ncol(review_more_strict) >= 1)
  
  review_less_strict <- TADA_AdditionalCharAliasForReview(
    displayPercent = TRUE,
    ATTAINS.WQX.tolerance = 0.5,
    WQX.ATTAINS.tolerance = 0.5
  )
  expect_s3_class(review_less_strict, "data.frame")
  expect_true(ncol(review_less_strict) >= 1)
})

test_that("TADA_AdditionalCharAliasForReview throws error for invalid tolerance", {
  expect_error(TADA_AdditionalCharAliasForReview(ATTAINS.CST.tolerance = 1.1))
})

test_that("TADA_AdditionalCharAliasForReview handles edge cases", {
  expect_error(TADA_AdditionalCharAliasForReview(ATTAINS.CST.tolerance = -0.1))
  expect_error(TADA_AdditionalCharAliasForReview(WQX.ATTAINS.tolerance = 1.5))
})

# Test for TADA_GetATTAINSOrgIDsRef
test_that("TADA_GetATTAINSOrgIDsRef returns a data frame", {
  # Temporarily override the function that fetches data
  original_function <- rExpertQuery::EQ_DomainValues
  assignInNamespace("EQ_DomainValues", function(...) data.frame(org_id = c("1", "2"), name = c("Org1", "Org2")), ns = "rExpertQuery")
  
  result <- TADA_GetATTAINSOrgIDsRef()
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
  expect_equal(nrow(result), 2)
  
  # Restore the original function
  assignInNamespace("EQ_DomainValues", original_function, ns = "rExpertQuery")
})

# Test for TADA_GetATTAINSParamUseOrgRef
test_that("TADA_GetATTAINSParamUseOrgRef returns a data frame", {
  # Temporarily override the function that fetches data
  original_function <- rExpertQuery::EQ_NationalExtract
  assignInNamespace("EQ_NationalExtract", function(...) data.frame(
    organizationId = c("1", "2"),
    organizationName = c("Org1", "Org2"),
    organizationType = c("Type1", "Type2"),
    parameterName = c("Param1", "Param2"),
    useName = c("Use1", "Use2"),
    waterType = c("Water1", "Water2")
  ), ns = "rExpertQuery")
  
  result <- TADA_GetATTAINSParamUseOrgRef()
  expect_s3_class(result, "data.frame")
  expect_true(ncol(result) >= 1)
  expect_equal(nrow(result), 2)
  
  # Restore the original function
  assignInNamespace("EQ_NationalExtract", original_function, ns = "rExpertQuery")
})

# Additional tests for side effects
test_that("TADA_UpdateATTAINSParamToWQPCharRef writes to file", {
  # Use a temporary file to test writing
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  TADA_UpdateATTAINSParamToWQPCharRef()
  expect_true(file.exists(temp_file))
})

test_that("TADA_UpdateATTAINSOrgIDsRef writes to file", {
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))
  
  TADA_UpdateATTAINSOrgIDsRef()
  expect_true(file.exists(temp_file))
})
