#### Create objects to use in tests ####
new <- letters[2]
old <- letters[12]
old_upper <- toupper(old)
x <- letters[11:13]
x_NA <- c("", NA_character_, x)
x_out <- x
x_out[2] <- new
x_quoted <- paste_quoted(x)
x_rep <- rep(x, 2)
x_upper <- toupper(x)
x_upper_out <- x_upper
x_upper_out[2] <- new
x_mixed <- c(x, x_upper)
x_mixed_out <- c(x_out, x_upper_out)
x_NA_factor <- addNA(as.factor(x_NA))

msg_replace <- paste0("Replaced values '", old, "' with '", new, "'")
msg_replace_mult <- "Replaced values 'l', 'L' with 'b'"
warn_input_new <- paste0("checkinput::is_character(new, allow_empty = TRUE,",
                         " allow_NA = TRUE) is not TRUE")
warn_input_old <- paste0("checkinput::all_characters(old, allow_empty = TRUE,",
                         " allow_NA = TRUE) is not TRUE")
warn_match_case <- paste0("Values in 'x' are a case-insensitive match but not",
                          " a case-sensitive match to ")
warn_mult_old <- "Multiple values of 'old' ("
warn_none_old <- "None of the values of argument 'old' "

#### Test the examples ####

# All values in 'x' that match any value of 'old' are replaced by 'new'.
expect_message(
  expect_identical(
    replace_vals(x = rep(x, 2), old = x[3:2], new = new,
                 allow_multiple = TRUE),
    c("k", "b", "b", "k", "b", "b")),
  pattern = "Replaced values 'l', 'm' with 'b'", strict = TRUE, fixed = TRUE)

# Factor input to 'x' is handled by replacing the values and levels. The
# levels are not reordered
expect_message(
  expect_identical(
    replace_vals(x = as.factor(x), old = x[3:2], new = new,
                 allow_multiple = TRUE),
    factor(c("k", "b", "b"), levels = c("k", "b"))),
  pattern = "Replaced values 'l', 'm' with 'b'", strict = TRUE, fixed = TRUE)

# Case-insensitive matching is used if 'ignore_case' is TRUE, with a warning
# if 'signal_case_old' is '"warning"'
expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = new,
                 ignore_case = TRUE, signal_case_old = "warning"),
    "b"),
  pattern = paste0(warn_match_case, "'old' ('a'): 'A'"),
  strict = TRUE, fixed = TRUE)

# Case-sensitive matching is used if 'ignore_case' is FALSE. A warning is
# issued if no match is found and 'warn_absent' is 'TRUE'
expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = new,
                 ignore_case = FALSE, warn_absent = TRUE),
    "A"),
  pattern = paste0(warn_none_old, "('a') were found in 'x' ('A')"),
  strict = TRUE, fixed = TRUE)

# If 'allow_multiple' is FALSE, an error is thrown if multiple values of
# 'old' match 'x'.
expect_error(
  replace_vals(x = x, old = letters[13:12], new = new,
               allow_multiple = FALSE),
  pattern = paste0(warn_mult_old, "'m', 'l') matched 'x' (", x_quoted,
                   "): 'l', 'm'"), fixed = TRUE)

#### Tests ####
##### ignore_case #####
# Single case-sensitive match to a single 'old'
expect_message(
  expect_identical(
    replace_vals(x = x, old = old, new = new, quiet = FALSE),
    x_out),
  pattern = msg_replace, strict = TRUE, fixed = TRUE)

# Multiple case-sensitive matches to a single 'old'
expect_message(
  expect_identical(
    replace_vals(x = x_rep, old = old, new = new, quiet = FALSE),
    rep(x_out, 2)),
  pattern = msg_replace, strict = TRUE, fixed = TRUE)

# A case-sensitive match and a case-insensitive match to a single 'old' (be
# quiet about that), which is allowed despite 'allow_multiple' being FALSE.
for(allow_multiple in c(TRUE, FALSE)) {
  expect_message(
    expect_identical(
      replace_vals(x = x_mixed, old = old, new = new,
                   ignore_case = TRUE, allow_multiple = allow_multiple,
                   signal_case_old = "quiet"),
      x_mixed_out),
    pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)

  expect_message(
    expect_identical(
      replace_vals(x = as.factor(x_mixed), old = old, new = new,
                   ignore_case = TRUE, allow_multiple = allow_multiple,
                   signal_case_old = "quiet"),
      factor(x_mixed_out, levels = c("k", "K", "b", "m", "M"))),
    pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)
}

# Multiple case-insensitive matches to a single 'old' that are not
# case-sensitive matches
expect_message(
  expect_identical(
    replace_vals(x = toupper(x_rep), old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE),
    rep(x_upper_out, 2)),
  pattern = paste0("Replaced values '", toupper(old), "' with '", new, "'"))

expect_message(
  expect_identical(
    replace_vals(x = x_rep, old = old_upper, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE),
    rep(x_out, 2)),
  pattern = paste0("Replaced values '", old, "' with '", new, "'"))

##### warn_absent #####
# No match (be quiet about that)
expect_silent(
  expect_identical(
    replace_vals(x = x, old = "v", new = new, warn_absent = FALSE),
    x))

# No match (warn about that)
expect_warning(
  expect_identical(
    replace_vals(x = x, old = "v", new = new, warn_absent = TRUE),
    x),
  pattern = paste0(warn_none_old, "('v') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE)

# Not warning about absence of 'old' if 'new' is present in 'x', even if
# 'warn_absent' is TRUE
expect_silent(
  expect_identical(
    replace_vals(x = x, old = "v", new = "k",
                 ignore_case = TRUE, warn_absent = TRUE, quiet = FALSE),
    x))

# Warn about absence of 'old' if 'warn_absent' is TRUE, even if a
# case-insensitive match (but not a case-sensitive match) of 'new' is present in
# 'x' and 'ignore_case' is TRUE
expect_warning(
  expect_identical(
    replace_vals(x = x, old = "v", new = "K",
                 ignore_case = TRUE, warn_absent = TRUE),
    x),
  pattern = paste0(warn_match_case, "'new' ('K'): 'k'"),
  strict = TRUE, fixed = TRUE)
expect_warning(
  expect_identical(
    replace_vals(x = x, old = "v", new = "K",
                 ignore_case = TRUE, warn_absent = TRUE),
    x),
  pattern = paste0(warn_none_old, "('v') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = as.factor(x), old = "v", new = "K",
                 ignore_case = TRUE, warn_absent = TRUE),
    as.factor(x)),
  pattern = paste0(warn_match_case, "'new' ('K'): 'k'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = as.factor(x), old = "v", new = "K",
                 ignore_case = TRUE, warn_absent = TRUE),
    as.factor(x)),
  pattern = paste0(warn_none_old, "('v') were found in 'x' (", x_quoted, ")"),
  strict = TRUE, fixed = TRUE)

# Both 'old' and 'new' present in 'x'
expect_message(
  expect_identical(
    replace_vals(x = c(x, new), old = old, new = new,
                 ignore_case = TRUE, allow_multiple = FALSE, warn_absent = TRUE,
                 quiet = FALSE),
    letters[c(11, 2, 13, 2)]),
  pattern = msg_replace, strict = TRUE, fixed = TRUE)

##### signal_case_old #####
# A case-sensitive match, would be another case-insensitive match (error about
# that)
for(ignore_case in c(TRUE, FALSE)) {
  expect_error(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = ignore_case, signal_case_old = "error"),
    pattern = paste0(warn_match_case, "'old' ('k', 'L'): 'l'"), fixed = TRUE)

  expect_error(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = ignore_case, signal_case_old = "error"),
    pattern = paste0(warn_match_case, "'old' ('K', 'l'): 'L'"), fixed = TRUE)
}

# A case-sensitive match, would be another case-insensitive match (warn about
# that). NB. The message about replacement is not tested here.
expect_warning(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = TRUE, signal_case_old = "warning"),
    c("b", "b", "m")),
  pattern = paste0(warn_match_case, "'old' ('k', 'L'): 'l'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = TRUE, signal_case_old = "warning"),
    c("b", "b", "M")),
  pattern = paste0(warn_match_case, "'old' ('K', 'l'): 'L'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = FALSE, signal_case_old = "warning"),
    c("b", "l", "m")),
  pattern = paste0(warn_match_case, "'old' ('k', 'L'): 'l'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = FALSE, signal_case_old = "warning"),
    c("b", "L", "M")),
  pattern = paste0(warn_match_case, "'old' ('K', 'l'): 'L'"),
  strict = TRUE, fixed = TRUE)

# A case-sensitive match, would be another case-insensitive match (message about
# that). NB. The message about replacement is not tested here.
expect_message(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = TRUE, signal_case_old = "message"),
    c("b", "b", "m")),
  pattern = paste0(warn_match_case, "'old' ('k', 'L'): 'l'"),
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = TRUE, signal_case_old = "message"),
    c("b", "b", "M")),
  pattern = paste0(warn_match_case, "'old' ('K', 'l'): 'L'"),
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = FALSE, signal_case_old = "message"),
    c("b", "l", "m")),
  pattern = paste0(warn_match_case, "'old' ('k', 'L'): 'l'"),
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = FALSE, signal_case_old = "message"),
    c("b", "L", "M")),
  pattern = paste0(warn_match_case, "'old' ('K', 'l'): 'L'"),
  strict = TRUE, fixed = TRUE)

# A case-sensitive match, would be another case-insensitive match (be quiet
# about that).
expect_message(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = TRUE, signal_case_old = "quiet"),
    c("b", "b", "m")),
  pattern = "Replaced values 'k', 'l' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = TRUE, signal_case_old = "quiet"),
    c("b", "b", "M")),
  pattern = "Replaced values 'K', 'L' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x, old = c(x[1], old_upper), new = new,
                 ignore_case = FALSE, signal_case_old = "quiet"),
    c("b", "l", "m")),
  pattern = "Replaced values 'k' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_upper, old = c(x_upper[1], old), new = new,
                 ignore_case = FALSE, signal_case_old = "quiet"),
    c("b", "L", "M")),
  pattern = "Replaced values 'K' with 'b'", strict = TRUE, fixed = TRUE)

# Handle false-positive matches if tolower(old) is equal to tolower(new)
expect_silent(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "A",
                 ignore_case = TRUE, signal_case_old = "quiet", quiet = FALSE),
    "A"))

expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "A",
                 ignore_case = TRUE, signal_case_old = "warning"),
    "A"),
  pattern = paste0(warn_match_case, "'old' ('a'): 'A'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = "A",
                 ignore_case = FALSE, signal_case_old = "warning"),
    "A"),
  pattern = paste0(warn_match_case, "'old' ('a'): 'A'"),
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = "A", old = "a", new = new,
                 ignore_case = TRUE, signal_case_old = "warning"),
    "b"),
  pattern = paste0(warn_match_case, "'old' ('a'): 'A'"),
  strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = "A", old = "A", new = "a",
                 ignore_case = TRUE, signal_case_new = "quiet"),
    "a"), pattern = "Replaced values 'A' with 'a'", strict = TRUE, fixed = TRUE)

##### signal_case_new #####
for(ignore_case in c(TRUE, FALSE)) {
  expect_error(
    replace_vals(x = c("a", "B"), old = "a", new = new,
                 ignore_case = ignore_case, warn_absent = TRUE,
                 signal_case_new = "error"),
    pattern = paste0(warn_match_case, "'new' ('b'): 'B'"), fixed = TRUE)

  expect_warning(
    expect_identical(
      replace_vals(x = c("a", "B"), old = "a", new = new,
                   ignore_case = ignore_case, signal_case_new = "warning"),
      c("b", "B")),
    pattern = paste0(warn_match_case, "'new' ('b'): 'B'"),
    strict = TRUE, fixed = TRUE)

  expect_warning(
    expect_identical(
      replace_vals(x = as.factor(c("a", "B")), old = "a", new = new,
                   ignore_case = ignore_case, signal_case_new = "warning"),
      as.factor(c("b", "B"))),
    pattern = paste0(warn_match_case, "'new' ('b'): 'B'"),
    strict = TRUE, fixed = TRUE)

  expect_message(
    expect_identical(
      replace_vals(x = c("a", "B"), old = "a", new = new,
                   ignore_case = ignore_case, signal_case_new = "message"),
      c("b", "B")),
    pattern = paste0(warn_match_case, "'new' ('b'): 'B'"),
    strict = TRUE, fixed = TRUE)

  expect_message(
    expect_identical(
      replace_vals(x = c("a", "B"), old = "a", new = new,
                   ignore_case = ignore_case, signal_case_new = "quiet"),
      c("b", "B")),
    "Replaced values 'a' with 'b'", strict = TRUE, fixed = TRUE)
}

##### allow_multiple #####
# One element in 'old' found multiple times (which is fine even if
# 'allow_multiple' is 'FALSE'), others not found
for(allow_multiple in c(TRUE, FALSE)) {
  expect_message(
    expect_identical(
      replace_vals(x = x_rep, old = c(old, "v"), new = new,
                   ignore_case = ignore_case, allow_multiple = allow_multiple),
      rep(x_out, 2)),
    pattern = msg_replace, strict = TRUE, fixed = TRUE)
}

# Multiple values in 'old' found once, 'allow_multiple' is 'FALSE'
expect_error(
  replace_vals(x = x[1:2], old = rev(x), new = new,
               ignore_case = ignore_case, allow_multiple = FALSE),
  pattern = paste0(warn_mult_old, "'m', 'l', 'k') matched 'x'",
                   " ('k', 'l'): 'k', 'l'"), fixed = TRUE)

expect_error(
  replace_vals(x = as.factor(x[1:2]), old = rev(x), new = new,
               ignore_case = ignore_case, allow_multiple = FALSE),
  pattern = paste0(warn_mult_old, "'m', 'l', 'k') matched 'x'",
                   " ('k', 'l'): 'k', 'l'"), fixed = TRUE)

# Multiple values in 'old' found multiple times, 'allow_multiple' is 'TRUE'
expect_message(
  expect_identical(
    replace_vals(x = x[c(1:2, 1:2)], old = rev(x), new = new,
                 ignore_case = ignore_case, allow_multiple = TRUE),
    c("b", "b", "b", "b")),
  pattern = "Replaced values 'k', 'l' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x[c(1:2, 1:2)]), old = rev(x), new = new,
                 ignore_case = ignore_case, allow_multiple = TRUE),
    as.factor(c("b", "b", "b", "b"))),
  pattern = "Replaced values 'k', 'l' with 'b'", strict = TRUE, fixed = TRUE)

# One element in 'old' found multiple time, one found once, one not found,
# 'allow_multiple' is TRUE
expect_message(
  expect_identical(
    replace_vals(x = c("m", "l", "l"), old = rev(x), new = new,
                 ignore_case = ignore_case, allow_multiple = TRUE),
    c("b", "b", "b")),
  pattern = "Replaced values 'm', 'l' with 'b'", strict = TRUE, fixed = TRUE)

# Error if one element of 'old' has a case-sensitive match to 'x' and another
# element of 'old' has a case-insensitive match to 'x' if allow_multiple is FALSE
expect_warning(
  expect_error(
    replace_vals(x = c("p", "V", "z"), old = c("p", "v", "y"), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE,
                 signal_case_old = "warning"),
    pattern = paste0(warn_mult_old, "'p', 'v', 'y') matched 'x'",
                     " ('p', 'V', 'z'): 'p', 'V'"),
    fixed = TRUE),
  pattern = paste0(warn_match_case, "'old' ('p', 'v', 'y'): 'V'"), fixed = TRUE)

expect_warning(
  expect_error(
    replace_vals(x = as.factor(c("p", "V", "z")), old = c("p", "v", "y"), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE,
                 signal_case_old = "warning"),
    pattern = paste0(warn_mult_old, "'p', 'v', 'y') matched 'x'",
                     " ('p', 'V', 'z'): 'p', 'V'"),
    fixed = TRUE),
  pattern = paste0(warn_match_case, "'old' ('p', 'v', 'y'): 'V'"), fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = c("p", "V", "z"), old = c("p", "v", "y"), new = new,
                 ignore_case = TRUE, allow_multiple = TRUE,
                 signal_case_old = "warning"),
    c("b", "b", "z")),
  pattern = paste0(warn_match_case, "'old' ('p', 'v', 'y'): 'V'"), fixed = TRUE)

# Single element in old matches lower case value in x (not the upper case value
# because 'ignore_case' is FALSE), allow_multiple = FALSE
expect_message(
  expect_identical(
    replace_vals(x = x_mixed, old = c("q", old), new = new,
                 ignore_case = FALSE, allow_multiple = FALSE,
                 signal_case_old = "quiet"),
    c(x_out, x_upper)),
  pattern = msg_replace, strict = TRUE, fixed = TRUE)

# Single element in old matches upper case and lower case value in x,
# allow_multiple = FALSE (tested twice to test the warning and the message)
expect_warning(
  expect_identical(
    replace_vals(x = x_mixed, old = c("q", old), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE),
    x_mixed_out),
  pattern = paste0(warn_match_case, "'old' ('q', 'l'): 'L"),
  strict = TRUE, fixed = TRUE)
expect_message(
  expect_identical(
    replace_vals(x = x_mixed, old = c("q", old), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE),
    x_mixed_out),
  pattern = msg_replace_mult, strict = FALSE, fixed = TRUE)

# If 'ignore_case' is 'TRUE', values in 'old' that differ in their case are fine
# even if 'signal_case_old' is '"error"'; they may both match to 'x' even if
# 'allow_multiple' is FALSE
expect_message(
  expect_identical(
    replace_vals(x = x_mixed, old = c("l", "L"), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE,
                 signal_case_old = "error"),
    x_mixed_out),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x_mixed), old = c("l", "L"), new = new,
                 ignore_case = TRUE, allow_multiple = FALSE,
                 signal_case_old = "error"),
    factor(x = x_mixed_out, levels = c("k", "K", "b", "m", "M"))),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)

# If 'ignore_case' is 'FALSE', values in 'old' that differ in their case lead to
# to an error if they both match to 'x' and 'allow_multiple' is FALSE
expect_error(
  replace_vals(x = x_mixed, old = c("l", "L"), new = new,
               ignore_case = FALSE, allow_multiple = FALSE,
               signal_case_old = "warning"),
  pattern = paste0(warn_mult_old, "'l', 'L') matched 'x' (",
                   x_quoted, ", 'K', 'L', 'M'): 'l', 'L'"),
  fixed = TRUE)

expect_error(
  replace_vals(x = as.factor(x_mixed), old = c("l", "L"), new = new,
               ignore_case = FALSE, allow_multiple = FALSE,
               signal_case_old = "warning"),
  pattern = paste0(warn_mult_old, "'l', 'L') matched 'x'",
                   " ('k', 'K', 'l', 'L', 'm', 'M'): 'l', 'L'"),
  fixed = TRUE)

# If 'ignore_case' is 'FALSE', values in 'old' that differ in their case are
# fine if they both match to 'x' and 'allow_multiple' is TRUE
expect_message(
  expect_identical(
    replace_vals(x = x_mixed, old = c("l", "L"), new = new,
                 ignore_case = FALSE, allow_multiple = TRUE,
                 signal_case_old = "warning"),
    x_mixed_out),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = as.factor(x_mixed), old = c("l", "L"), new = new,
                 ignore_case = FALSE, allow_multiple = TRUE,
                 signal_case_old = "warning"),
    factor(x = x_mixed_out, levels = c("k", "K", "b", "m", "M"))),
  pattern = msg_replace_mult, strict = TRUE, fixed = TRUE)

##### NA and "" #####
expect_message(
  expect_identical(
    replace_vals(x = x_NA, old = "k", new = "b"),
    c("", NA_character_, "b", "l", "m")),
  pattern = "Replaced values 'k' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = "k", new = "b"),
    addNA(as.factor(c("", NA_character_, "b", "l", "m")))),
  pattern = "Replaced values 'k' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA, old = NA_character_, new = new),
    c("", "b", x)),
  pattern = "Replaced values 'NA' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = NA_character_, new = new),
    factor(c("", "b", x), levels = c("", x, "b"))),
  pattern = "Replaced values 'NA' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA, old = "", new = new),
    c("b", NA_character_, x)),
  pattern = "Replaced values '' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = "", new = new),
    addNA(factor(c("b", NA_character_, x), levels = c("b", x)))),
  pattern = "Replaced values '' with 'b'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA, old = "", new = NA_character_),
    c(NA_character_, NA_character_, x)),
  pattern = "Replaced values '' with 'NA'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = "", new = NA_character_),
    addNA(as.factor(c(NA_character_, NA_character_, x)))),
  pattern = "Replaced values '' with 'NA'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x, old = old, new = ""),
    c("k", "", "m")),
  pattern = "Replaced values 'l' with ''", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = old, new = ""),
    addNA(as.factor(c("", NA, "k", "", "m")))),
  pattern = "Replaced values 'l' with ''", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = c("", x), old = "", new = NA_character_),
    c(NA_character_, x)),
  pattern = "Replaced values '' with 'NA'", strict = TRUE, fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x_NA_factor, old = "", new = NA_character_),
    addNA(as.factor(c(NA_character_, NA_character_, x)))),
  pattern = "Replaced values '' with 'NA'", strict = TRUE, fixed = TRUE)

expect_warning(
  expect_identical(
    replace_vals(x = factor(x_NA, levels = x_NA, exclude = NULL),
                 old = "", new = NA_character_),
    addNA(as.factor(c(NA_character_, NA_character_, x)))),
  pattern = "'NA' will become the last level", strict = TRUE, fixed = TRUE)

##### Erroneous input #####
expect_error(
  replace_vals(x = NULL, old = old, new = new),
  pattern = "is.character(x) || is.factor(x) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = character(0), old = old, new = new),
  pattern = "length(x) > 0L is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = 1, old = old, new = new),
  pattern = "is.character(x) || is.factor(x) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = NULL, new = new),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = character(0), new = new),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = 1, new = new),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = as.factor(old), new = new),
  pattern = warn_input_old, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = c(old, old), new = new),
  pattern = paste0("values in 'old' (", paste_quoted(c(old, old)),
                   ") should be unique"), fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = NULL),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = character(0)),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = 1),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = as.factor(new)),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = c(new, new)),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = c("a", "A"), ignore_case = TRUE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = c("a", "A"), ignore_case = FALSE),
  pattern = warn_input_new, fixed = TRUE)

expect_error(
  replace_vals(x = "abc", old = c(old, new), new = new),
  pattern = paste0("'new' ('", new, "') should not be present in 'old' (",
                   paste_quoted(c(old, new)), ")"), fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = NA),
  pattern = "checkinput::is_logical(ignore_case) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, ignore_case = 1),
  pattern = "checkinput::is_logical(ignore_case) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, allow_multiple = NA),
  pattern = "checkinput::is_logical(allow_multiple) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, allow_multiple = 1),
  pattern = "checkinput::is_logical(allow_multiple) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, warn_absent = NA),
  pattern = "checkinput::is_logical(warn_absent) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, warn_absent = 1),
  pattern = "checkinput::is_logical(warn_absent) is not TRUE", fixed = TRUE)

# match.arg() uses partial matching (!)
expect_error(
  replace_vals(x = x, old = "K", new = "l", signal_case_old = "e"),
  pattern = paste0(warn_match_case, "'old' ('K'): 'k'"), fixed = TRUE)

expect_message(
  expect_identical(
    replace_vals(x = x,  old = "K", new = "l", signal_case_old = "m"),
    x),
  pattern = paste0(warn_match_case, "'old' ('K'): 'k'"), fixed = TRUE)

expect_error(
  replace_vals(x = x,  old = "K", new = "l", signal_case_old = "abc"),
  pattern = "warning.+quiet", fixed = FALSE, ignore.case = FALSE)

expect_error(
  replace_vals(x = x, old = old, new = "K", signal_case_new = "e"),
  pattern = paste0(warn_match_case, "'new' ('K'): 'k'"), fixed = TRUE)

expect_message(
  replace_vals(x = x, old = old, new = new, signal_case_new = "m"),
  pattern = msg_replace, strict = TRUE, fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, signal_case_new = "abc"),
  pattern = "warning.+quiet", fixed = FALSE, ignore.case = FALSE)

expect_error(
  replace_vals(x = x, old = old, new = new, quiet = NA),
  pattern = "checkinput::is_logical(quiet) is not TRUE", fixed = TRUE)

expect_error(
  replace_vals(x = x, old = old, new = new, quiet = 1),
  pattern = "checkinput::is_logical(quiet) is not TRUE", fixed = TRUE)


#### Remove objects used in tests ####
rm(allow_multiple, ignore_case, msg_replace, msg_replace_mult, new, old,
   old_upper, warn_input_new, warn_input_old, warn_match_case, warn_mult_old,
   warn_none_old, x, x_mixed, x_mixed_out, x_NA, x_NA_factor, x_out, x_quoted,
   x_rep, x_upper, x_upper_out)
