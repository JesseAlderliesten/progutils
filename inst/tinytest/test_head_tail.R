#### Create objects to use in tests ####
x <- letters
names(x) <- LETTERS
x[1] <- NA_integer_
x_out <- x[c(1:4, 23:26)]
m <- matrix(data = 1:40, ncol = 4,
            dimnames = list(LETTERS[1:10], letters[1:4]))
m[1, 2] <- NA_integer_
m_out <- m[c(1:3, 8:10), , drop = FALSE]
test_num <- c(11, NA, 13, 14)
test_char <- c("k", NA_character_, "m", "n")
test_list <- list(num1 = test_num, num2 = rev(test_num), num3 = test_num,
                  char1 = test_char, char2 = rev(test_char), char3 = test_char)


#### Tests ####
expect_silent(expect_identical(head_tail(x = 3, n = 1), 3))
expect_silent(expect_identical(head_tail(x = 3L, n = 1), 3L))
expect_silent(expect_identical(head_tail(x = 3, n = 5), 3))
expect_silent(expect_identical(head_tail(x = 3L, n = 5), 3L))

expect_silent(expect_identical(head_tail(x, n = 1L), x_out[c(1L, length(x_out))]))
expect_silent(expect_identical(head_tail(x), x[c(1:3, 24:26)]))
expect_silent(expect_identical(head_tail(x, n = 4 - 1e-9), x_out))
expect_silent(expect_identical(head_tail(x, n = 4), x_out))
expect_silent(expect_identical(head_tail(x, n = 4 + 1e-9), x_out))
expect_silent(expect_identical(head_tail(x, n = 40), x))

expect_silent(expect_identical(head_tail(m, n = 1L), m_out[c(1L, nrow(m_out)), ]))
expect_silent(expect_identical(head_tail(m), m_out))

expect_silent(expect_identical(head_tail(as.data.frame(m), n = 1L),
                               as.data.frame(m_out[c(1L, nrow(m_out)), ])))
expect_silent(expect_identical(head_tail(as.data.frame(m)), as.data.frame(m_out)))

expect_silent(expect_identical(head_tail(x = test_list, n = 1), test_list[c(1, 6)]))
expect_silent(expect_identical(head_tail(x = test_list), test_list))

expect_silent(expect_identical(head_tail(x = NULL, n = 1), NULL))
expect_silent(expect_identical(head_tail(x = character(0L), n = 1), character(0L)))
expect_silent(expect_identical(head_tail(x = numeric(0L), n = 1), numeric(0L)))


##### Erroneous input #####
expect_error(
  head_tail(x, n = 0),
  pattern = "checkinput::all_natural(n) is not TRUE", fixed = TRUE)

expect_error(
  head_tail(x, n = 4.1),
  pattern = "checkinput::all_natural(n) is not TRUE", fixed = TRUE)

expect_error(
  head_tail(x, n = "a"),
  pattern = "checkinput::all_natural(n) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(m, m_out, test_char, test_list, test_num, x, x_out)
