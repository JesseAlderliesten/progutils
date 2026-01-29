#### Create objects to use in tests ####
current_R <- as.character(getRversion())
string_R <- paste0("R-", current_R)
old_R <- paste(as.integer(R.version$major) - 1L, R.version$minor, sep = ".")

paths_test <- c(
  xyz_current = paste0("C:/Program Files/R/R-", current_R, "/library"),
  xyz_old = paste0("C:/Program Files/R/R-", old_R, ", /library"),
  xy_current = paste0("C:/Users/Eigenaar/AppData/Local/R/win-library/",
                      substr(x = current_R, start = 1, stop = 3)),
  xy_old = paste0("C:/Users/Eigenaar/AppData/Local/R/win-library/",
                  substr(x = old_R, start = 1, stop = 3)))


#### Test the examples ####
expect_true(all(.libPaths() %in% get_paths()))
expect_true(all(get_paths() %in% .libPaths()))


#### Tests ####
paths_in <- paths_test[1:4]
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             paths_test[1:4])

paths_in <- paths_test[4:1]
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             paths_test[c(1, 4:2)])

paths_in <- paths_test[c(3, 4, 1, 2)]
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             paths_test[c(1, 3, 4, 2)])

paths_in <- c(ok = "ab", empty = "", "NA" = NA_character_, paths_test[c(2:1)])
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(bool_Rversion, c(rep(FALSE, 4L), TRUE))
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             c(paths_in[5], paths_in[1:4]))

paths_in <- c(ok = "ab", empty = "", "NA" = NA_character_, paths_test[2])
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(bool_Rversion, c(rep(FALSE, 4L)))
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             paths_in)

paths_in <- rep(paths_test[1], 2L)
bool_Rversion <- grepl(pattern = string_R, x = paths_in, fixed = TRUE)
expect_equal(bool_Rversion, c(rep(TRUE, 2L)))
expect_equal(c(paths_in[bool_Rversion], paths_in[!bool_Rversion]),
             paths_in)


#### Remove objects used in tests ####
rm(bool_Rversion, current_R, old_R, paths_in, paths_test, string_R)
