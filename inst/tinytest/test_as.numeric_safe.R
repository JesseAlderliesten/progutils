#### Create objects to use in tests ####
int_example <- 5:7
char_example <- as.character(int_example)
fact_example <- as.factor(int_example)
num_example <- as.numeric(int_example)
out_NA_example <- rep(NA_real_, 3L)

x_char_mix <- c("1.0", "-3", "NA", "Inf", "5.0", "1.3", "-1.3", "13.4", "0.013")
x_num <- c(1, 20, 20, 5, 5)
out_num_mix <- c(1, -3, NA, Inf, 5, 1.3, -1.3, 13.4, 0.013)

as_num_fact <- function(x) {as.numeric(levels(x))[x]}
as_num_7.10 <- function(x) {as.numeric(levels(x))[as.integer(x)]}
as_num_base <- function(x) {as.numeric(x)}


#### Test the examples ####
# as.numeric_safe() works irrespective the type of x
expect_identical(as.numeric_safe(int_example), num_example)
expect_identical(as.numeric_safe(num_example), num_example)
expect_identical(as.numeric_safe(char_example), num_example)
expect_identical(as.numeric_safe(fact_example), num_example)

# The 'more efficient' suggestion in help(factor) *only* works for factors.
expect_identical(as_num_fact(int_example), out_NA_example)
expect_identical(as_num_fact(num_example), out_NA_example)
expect_identical(as_num_fact(char_example), out_NA_example)
expect_identical(as_num_fact(fact_example), num_example)

# The 'more efficient' suggestion in R FAQ 7.10 *only* works for factors.
expect_identical(as_num_7.10(int_example), out_NA_example)
expect_identical(as_num_7.10(num_example), out_NA_example)
expect_identical(as_num_7.10(char_example), out_NA_example)
expect_identical(as_num_7.10(fact_example), num_example)

# as.numeric() gives the *indices* of the factor levels for factors
expect_identical(as_num_base(int_example), num_example)
expect_identical(as_num_base(num_example), num_example)
expect_identical(as_num_base(char_example), num_example)
expect_identical(as_num_base(fact_example), c(1, 2, 3))


#### Tests ####
expect_silent(expect_equal(as.numeric_safe(as.character(x_num)), x_num))
expect_silent(expect_equal(as.numeric_safe(as.factor(as.character(x_num))), x_num))
expect_silent(expect_equal(as.numeric_safe(x_num), x_num))

expect_warning(expect_equal(as.numeric_safe(x_char_mix), out_num_mix),
               pattern = "NAs introduced by coercion", strict = TRUE, fixed = TRUE)
expect_warning(expect_equal(as.numeric_safe(as.factor(x_char_mix)), out_num_mix),
               pattern = "NAs introduced by coercion", strict = TRUE, fixed = TRUE)
expect_silent(expect_equal(as.numeric_safe(out_num_mix), out_num_mix))
# input with NA_character_ instead of "NA"
expect_silent(expect_equal(
  as.numeric_safe(as.character(out_num_mix)),
  out_num_mix))

expect_silent(expect_equal(
  as.numeric_safe(factor(x = c(-1, 1, NA, Inf, NaN), exclude = NULL)),
  c(-1, 1, NA, Inf, NaN))) # Keep 'NA_character_' as a level

expect_warning(expect_equal(
  as.numeric_safe(c(1:3, letters[1:3])),
  c(1:3, rep(NA_real_, 3L))),
  pattern = "NAs introduced by coercion", strict = TRUE, fixed = TRUE)
expect_warning(expect_equal(
  as.numeric_safe(as.factor(c(1:3, letters[1:3]))),
  c(1:3, rep(NA_real_, 3L))),
  pattern = "NAs introduced by coercion", strict = TRUE, fixed = TRUE)

expect_silent(expect_equal(as.numeric_safe(NULL), numeric(0)))
expect_silent(expect_equal(as.numeric_safe(character(0)), numeric(0)))
expect_silent(expect_equal(
  as.numeric_safe(factor(1:3, levels = 1:2)),
  c(1, 2, NA)))
expect_silent(expect_equal(
  as.numeric_safe(factor(1:3, levels = 1:4)),
  c(1, 2, 3)))
expect_warning(expect_equal(
  as.numeric_safe(c(TRUE, FALSE, NA)),
  c(NA_real_, NA_real_, NA_real_)),
  pattern = "NAs introduced by coercion", strict = TRUE, fixed = TRUE)


#### Remove objects used in tests ####
rm(as_num_7.10, as_num_base, as_num_fact, out_num_mix, char_example, x_char_mix,
   fact_example, int_example, x_num, num_example, out_NA_example)
