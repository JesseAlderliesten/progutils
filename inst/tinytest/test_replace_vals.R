#### Create objects to use in tests ####
match_mult_quoted <- "'k', 'K', 'm', 'M'"
new <- letters[2]
old <- letters[12]
old_mult <- letters[11:12]
old_mult_quoted <- paste_quoted(old_mult)
x <- letters[11:13]
x_quoted <- paste_quoted(x)
x_rep <- rep(x, 2)
x_rep_quoted <- paste_quoted(x_rep)
x_alt <- c(rev(x), toupper(rev(x)), x[3])
x_alt_quoted <- paste_quoted(x_alt)
x_factor <- as.factor(c("a", "c", "", "e"))
x_factor_NA <- addNA(as.factor(c("a", NA_character_, "c", "", "e")))

error_mult_old <- "Multiple matches to 'old' were found: "
msg_replace <- paste0("Replaced values '", old, "' with '", new, "'")
msg_replace_mult <- paste0("Replaced values ", old_mult_quoted, " with '", new, "'")
warn_input_new <- "checkinput::is_character(new, allow_empty = TRUE, allow_NA = TRUE) is not TRUE"
warn_input_old <- "checkinput::all_characters(old, allow_empty = TRUE, allow_NA = TRUE) is not TRUE"
warn_none_old <- "None of the values of argument 'old' "


#### Test the examples ####
# Examples are tested in 'Tests' below.


#### Tests ####
##### Single value in 'old' #####
# Not found
expect_warning(
  expect_identical(
    replace_vals(x = x, old = "v", new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    x),
  pattern = paste0(warn_none_old, "('v') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    replace_vals(x = x, old = "v", new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = FALSE,
                 quiet = FALSE),
    x)
)

# Found once
expect_message(
  expect_identical(
    replace_vals(x = x, old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    letters[c(11, 2, 13)]),
  pattern = msg_replace, strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    replace_vals(x = x, old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = TRUE),
    letters[c(11, 2, 13)])
)

# Found multiple times
expect_message(
  expect_identical(
    replace_vals(x = letters[c(12, 12:13)], old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    letters[c(2, 2, 13)]),
  pattern = msg_replace, strict = TRUE, fixed = TRUE
)

##### Multiple values in 'old' #####
# None found, warn_absent = TRUE
expect_warning(
  expect_identical(
    replace_vals(x = x, old = letters[22:23], new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    x),
  pattern = paste0(warn_none_old, "('v', 'w') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE
)

# None found, warn_absent = FALSE
expect_silent(
  expect_identical(
    replace_vals(x = x, old = letters[22:23], new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = FALSE,
                 quiet = FALSE),
    x)
)

# Each found once, allow_multiple = FALSE
expect_error(
  replace_vals(x = x, old = old_mult, new = new,
               ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
               quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", old_mult_quoted,
                   ") were found in 'x' (", x_quoted, "): 'k', 'l'"),
  fixed = TRUE
)

# Each found once, allow_multiple = TRUE
expect_message(
  expect_identical(
    replace_vals(x = x, old = old_mult, new = new,
                 ignore_case = TRUE, allow_multiple = TRUE, warn_absent = TRUE,
                 quiet = FALSE),
    letters[c(2, 2, 13)]),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    replace_vals(x = x, old = old_mult, new = new,
                 ignore_case = TRUE, allow_multiple = TRUE, warn_absent = TRUE,
                 quiet = TRUE),
    letters[c(2, 2, 13)])
)

# One found multiple time, others not found
expect_message(
  expect_identical(
    replace_vals(x = x_rep, old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    rep(letters[c(11, 2, 13)], 2)),
  pattern = msg_replace, strict = TRUE, fixed = TRUE
)

# One found multiple time, one found once, one not found, 'allow_multiple' is FALSE
expect_error(
  replace_vals(x = letters[c(12:13, 13)], old = letters[13:11], new = new,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", paste_quoted(letters[13:11]),
                   ") were found in 'x' (", paste_quoted(letters[c(12:13, 13)]),
                   "): 'l', 'm'"),
  fixed = TRUE)

# One found multiple time, one found once, one not found, 'allow_multiple' is TRUE
expect_message(
  expect_identical(
    replace_vals(x = letters[c(12:13, 13)], old = letters[13:11], new = new,
                 allow_multiple = TRUE, warn_absent = TRUE, quiet = FALSE),
    letters[rep(2, 3)]), pattern = "Replaced values 'l', 'm' with 'b'",
  strict = TRUE, fixed = TRUE)

##### Handling case #####
for(warn_newcase in c("always", "ignore_case")) {
  expect_warning(
    expect_identical(
      replace_vals(x = "B", old = "a", new = "b", ignore_case = TRUE,
                   allow_multiple = FALSE, warn_absent = TRUE,
                   warn_newcase = warn_newcase, quiet = FALSE),
      "B"),
    pattern = paste0("Values in 'x' ('B') are a case-insensitive match but not a",
                     " case-sensitive match to 'new' ('b'): 'B'"),
    strict = TRUE, fixed = TRUE)
}

expect_warning(
  expect_identical(
    replace_vals(x = "B", old = "a", new = "b", ignore_case = TRUE,
                 allow_multiple = FALSE, warn_absent = TRUE,
                 warn_newcase = "never", quiet = FALSE),
    "B"),
  pattern = paste0(warn_none_old, "('a') were found in 'x' ('B')"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = "B", old = "a", new = "b", ignore_case = FALSE,
                 allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
    "B"),
  pattern = paste0(warn_none_old, "('a') were found in 'x' ('B')"),
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "b", ignore_case = TRUE,
                 allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
    "b"),
  pattern = "Replaced values 'A' with 'b'", strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "b", ignore_case = FALSE,
                 allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
    "A"),
  pattern = paste0(warn_none_old, "('a') were found in 'x' ('A')"),
  strict = TRUE, fixed = TRUE)

# Same for 'ignore_case' TRUE and FALSE
for(ignore_case in c(TRUE, FALSE)) {
  expect_message(
    expect_identical(
      replace_vals(x = "a", old = "a", new = "b", ignore_case = ignore_case,
                   allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
      "b"),
    pattern = "Replaced values 'a' with 'b'",
    strict = TRUE, fixed = TRUE)

  expect_silent(
    expect_identical(
      replace_vals(x = "A", old = "a", new = "A", ignore_case = TRUE,
                   allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
      "A")
  )
}

expect_error(
  replace_vals(x = x_alt, old = c("M", "k"), new = "x", ignore_case = TRUE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' ('M', 'k') were found in 'x' (",
                   x_alt_quoted, "): ", match_mult_quoted), fixed = TRUE
)

expect_message(
  expect_identical(
    replace_vals(x = x_alt, old = c("M", "k"), new = "x", ignore_case = TRUE,
                 allow_multiple = TRUE, warn_absent = TRUE, quiet = FALSE),
    c("x", "l", "x", "x", "L", "x", "x")),
  pattern = paste0("Replaced values ", match_mult_quoted, " with 'x'"),
  strict = TRUE, fixed = TRUE
)

expect_error(
  replace_vals(x = x_alt, old = c("M", "k"), new = "x", ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' ('M', 'k') were found in 'x' (",
                   x_alt_quoted, "): 'k', 'M'"), fixed = TRUE
)

expect_message(
  expect_identical(
    replace_vals(x = x_alt, old = c("M", "k"), new = "x", ignore_case = FALSE,
                 allow_multiple = TRUE, warn_absent = TRUE, quiet = FALSE),
    c("m", "l", "x", "x", "L", "K", "m")),
  pattern = "Replaced values 'k', 'M' with 'x'", strict = TRUE, fixed = TRUE
)

# Catch false-positive matches if tolower(old) is equal to tolower(new)
expect_silent(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "A", ignore_case = TRUE),
    "A"))

##### Factor input #####
expect_error(
  replace_vals(x = as.factor(x), old = old_mult, new = new, ignore_case = TRUE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", old_mult_quoted,
                   ") were found in 'x' (", x_quoted), fixed = TRUE
)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x), old = old_mult, new = new, ignore_case = TRUE,
                 allow_multiple = TRUE, warn_absent = TRUE, quiet = FALSE),
    as.factor(letters[c(2, 2, 13)])),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE
)

expect_warning(
  expect_identical(
    replace_vals(x = as.factor(x), old = letters[22:23], new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    as.factor(x)),
  pattern = paste0(warn_none_old, "('v', 'w') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE
)

expect_silent(
  expect_identical(
    replace_vals(x = as.factor(x), old = letters[22:23], new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = FALSE,
                 quiet = FALSE),
    as.factor(x))
)

expect_error(
  replace_vals(x = as.factor(letters[c(12:13, 13)]), old = letters[13:11], new = new,
               ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
               quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", paste_quoted(letters[13:11]),
                   ") were found in 'x' (",
                   paste_quoted(levels(as.factor(letters[c(12:13, 13)]))),
                   "): 'l', 'm'"),
  fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(letters[c(12:13, 13)]), old = letters[13:11], new = new,
                 ignore_case = TRUE, allow_multiple = TRUE, warn_absent = TRUE,
                 quiet = FALSE),
    as.factor(letters[rep(2, 3)])),
  pattern = "Replaced values 'l', 'm' with 'b'", strict = TRUE, fixed = TRUE)

expect_silent(
  expect_identical(
    replace_vals(x = as.factor("A"), old = "a", new = "A", ignore_case = FALSE,
                 allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
    as.factor("A"))
)

expect_error(
  replace_vals(x = as.factor(x_alt), old = c("M", "k"), new = "x",
               ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
               quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", paste_quoted(c("M", "k")),
                   ") were found in 'x' (", paste_quoted(levels(as.factor(x_alt))),
                   "): ", match_mult_quoted), fixed = TRUE
)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x_alt), old = c("M", "k"), new = "x",
                 ignore_case = TRUE, allow_multiple = TRUE, warn_absent = TRUE,
                 quiet = FALSE),
    factor(x = c("x", "l", "x", "x", "L", "x", "x"), levels = c("x", "l", "L"))),
  pattern = paste0("Replaced values ", match_mult_quoted, " with 'x'"),
  strict = TRUE, fixed = TRUE
)

expect_error(
  replace_vals(x = as.factor(x_alt), old = c("M", "k"), new = "x",
               ignore_case = FALSE, allow_multiple = FALSE, warn_absent = TRUE,
               quiet = FALSE),
  pattern = paste0("Multiple matches to 'old' (", paste_quoted(c("M", "k")),
                   ") were found in 'x' (", paste_quoted(levels(as.factor(x_alt))),
                   "): 'k', 'M'"), fixed = TRUE
)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x_alt), old = c("M", "k"), new = "x",
                 ignore_case = FALSE, allow_multiple = TRUE, warn_absent = TRUE,
                 quiet = FALSE),
    factor(x = c("m", "l", "x", "x", "L", "K", "m"),
           levels = c("x", "K", "l", "L", "m"))),
  pattern = "Replaced values 'k', 'M' with 'x'", strict = TRUE, fixed = TRUE
)

##### NA and "": character input #####
expect_message(
  expect_identical(
    replace_vals(x = c("a", NA_character_, "c", "", "e"),
                 old = NA_character_, new = "x"),
    c("a", "x", "c", "", "e")),
  pattern = "Replaced values 'NA' with 'x'",
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = c("a", NA_character_, "c", "", "e"), old = "", new = "x"),
    c("a", NA_character_, "c", "x", "e")),
  pattern = "Replaced values '' with 'x'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = c("a", NA_character_, "c", "", "e"),
                 old = "", new = NA_character_),
    c("a", NA_character_, "c", NA_character_, "e")),
  pattern = "Replaced values '' with 'NA'",
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = c("a", NA_character_, "c", "", "e"), old = "a", new = ""),
    c("", NA_character_, "c", "", "e")),
  pattern = "Replaced values 'a' with ''", strict = TRUE, fixed = TRUE)

##### NA and "": factor input #####
expect_message(
  expect_identical(
    replace_vals(x = x_factor_NA, old = NA_character_, new = "x"),
    factor(c("a", "x", "c", "", "e"), levels = c("", "a", "c", "e", "x"))),
  pattern = "Replaced values 'NA' with 'x'",
  strict = TRUE, fixed = TRUE)

# No need to warn about adding level 'NA' if it already was the last level.
expect_message(
  expect_identical(
    replace_vals(x = x_factor_NA, old = "", new = "x"),
    addNA(factor(c("a", NA_character_, "c", "x", "e"),
                 levels = c("x", "a", "c", "e")))),
  pattern = "Replaced values '' with 'x'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_factor_NA, old = "", new = NA_character_),
    addNA(as.factor(c("a", NA_character_, "c", NA_character_, "e")))),
  pattern = "Replaced values '' with 'NA'",
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = reorder_levels(x = x_factor_NA,
                                    new_order = levels(x_factor_NA)[c(1:3, 5, 4)]),
                 old = "", new = NA_character_),
    addNA(as.factor(c("a", NA_character_, "c", NA_character_, "e")))),
  pattern = "'NA' will become the last level", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_factor, old = "", new = NA_character_),
    addNA(as.factor(c("a", "c", NA_character_, "e")))),
  pattern = "Replaced values '' with 'NA'",
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_factor_NA, old = "a", new = ""),
    addNA(as.factor(c("", NA_character_, "c", "", "e")))),
  pattern = "Replaced values 'a' with ''", strict = TRUE, fixed = TRUE)

##### Erroneous input #####
expect_error(
  replace_vals(x = NULL, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = "is.character(x) || is.factor(x) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = character(0), old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = "length(x) > 0L is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = 1, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = "is.character(x) || is.factor(x) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = NULL, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = character(0), new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = 1, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = as.factor(old), new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = c(old_mult, old, old), new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = "values in 'old' should be unique", fixed = TRUE)

expect_error(
  replace_vals(x = "abc", old = c(old_mult, new), new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = paste0("'new' ('", new, "') should not be present in 'old' (",
                   paste_quoted(c(old_mult, new)), ")"), fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = NULL, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = character(0), ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = 1, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = as.factor(new), ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = c("a", "A"), ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = c("a", "A"), ignore_case = TRUE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = NA,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = FALSE),
  pattern = "checkinput::is_logical(ignore_case) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = FALSE,
               allow_multiple = NA, warn_absent = TRUE, quiet = FALSE),
  pattern = "checkinput::is_logical(allow_multiple) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = NA, quiet = FALSE),
  pattern = "checkinput::is_logical(warn_absent) is not TRUE", fixed = TRUE)

# match.arg() uses partial matching (!)
expect_message(
  replace_vals(x = x, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, warn_newcase = "a",
               quiet = FALSE),
  pattern = msg_replace, fixed = TRUE)

# match.arg() warns not to check the warning message.
expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, warn_newcase = "q",
               quiet = FALSE),
  pattern = "always.+never", fixed = FALSE, ignore.case = FALSE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = FALSE,
               allow_multiple = FALSE, warn_absent = TRUE, quiet = NA),
  pattern = "checkinput::is_logical(quiet) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(error_mult_old, match_mult_quoted, msg_replace, msg_replace_mult, new, old,
   old_mult, old_mult_quoted, warn_input_new, warn_input_old, warn_none_old, x,
   x_alt, x_alt_quoted, x_factor, x_factor_NA, x_quoted, x_rep, x_rep_quoted)
