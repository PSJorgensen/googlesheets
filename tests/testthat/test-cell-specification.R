context("cell specification")

test_that("Column letter converts to correct column number", {
  
  expect_equal(letter_to_num("A"), 1)
  expect_equal(letter_to_num("AB"), 28)
  expect_equal(letter_to_num(c("A", "AH", "ABD", "XFD")), c(1, 34, 732, 16384))
  
})

test_that("Column number converts to correct column letter", {
  
  expect_equal(num_to_letter(1), "A")
  expect_equal(num_to_letter(28), "AB")
  expect_equal(num_to_letter(c(1, 34, 732, 16384)), c("A", "AH", "ABD", "XFD"))
  
})

test_that("A1 notation converts to R1C1 notation", {
  
  expect_equal(A1_to_RC("A1"), "R1C1")
  expect_equal(A1_to_RC("AB10"), "R10C28")
  expect_equal(A1_to_RC(c("A1", "ZZ100", "ZZZ15")),
               c("R1C1", "R100C702", "R15C18278"))
  
})

test_that("R1C1 notation converts to A1 notation", {
  
  expect_equal(RC_to_A1("R1C1"), "A1")
  expect_equal(RC_to_A1("R10C28"), "AB10")
  expect_equal(RC_to_A1(c("R1C1", "R100C702", "R15C18278")),
               c("A1", "ZZ100", "ZZZ15"))
  
})


test_that("Cell range is converted to a limit list and vice versa", {
  
  rgA1 <- "A1:C4"
  rgRC <- "R1C1:R4C3"
  rgLL <- list(`min-row` = 1, `min-col` = 1, `max-row` = 4, `max-col` = 3)
  expect_equal(convert_range_to_limit_list(rgA1), rgLL)
  expect_equal(convert_limit_list_to_range(rgLL), rgA1)
  expect_equal(convert_limit_list_to_range(rgLL, rc = TRUE), rgRC)
  
  rgA1 <- "E7"
  rgA1A1 <- "E7:E7"
  rgRC <- "R7C5"
  rgRCRC <- "R7C5:R7C5"
  rgLL <- list(`min-row` = 7, `min-col` = 5, `max-row` = 7, `max-col` = 5)
  expect_equal(convert_range_to_limit_list(rgA1), rgLL)
  expect_equal(convert_limit_list_to_range(rgLL), rgA1A1)
  expect_equal(convert_limit_list_to_range(rgLL, rc = TRUE), rgRCRC)
  
})
