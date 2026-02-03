#### Create objects to use in tests ####
current_R <- as.character(getRversion())
string_R <- paste0("R-", current_R)
old_R <- paste(as.integer(R.version$major) - 1L, R.version$minor, sep = ".")

paths <- c(
  xyz_current = paste0("C:/Program Files/R/R-", current_R, "/library"),
  xy_current = paste0("C:/Users/Eigenaar/AppData/Local/R/win-library/",
                      substr(x = current_R, start = 1, stop = 3)),
  xyz_old = paste0("C:/Program Files/R/R-", old_R, "/library"),
  xy_old = paste0("C:/Users/Eigenaar/AppData/Local/R/win-library/",
                  substr(x = old_R, start = 1, stop = 3)))
paths_alt <- c(no_R = "ab", empty = "", "NA" = NA_character_)


#### Test the examples ####
expect_true(all(.libPaths() %in% get_paths(paths = .libPaths())))
expect_true(all(get_paths(paths = .libPaths()) %in% .libPaths()))


#### Tests ####
expect_silent(
  expect_equal(get_paths(paths = paths[1:4]), paths)
)
expect_silent(
  expect_equal(get_paths(paths = paths[c(3:4, 1:2)]), paths)
)
expect_silent(
  expect_equal(get_paths(paths = paths[4:1]), paths[c(2:1, 4:3)])
)

expect_silent(
  expect_equal(get_paths(paths = c(paths_alt, paths[c(2:1)])),
               c(paths[2:1], paths_alt))
)

expect_silent(
  expect_equal(get_paths(paths = c(paths_alt, paths[2])), c(paths[2], paths_alt))
)

expect_silent(
  expect_equal(get_paths(paths = rep(paths[1], 2L)), rep(paths[1], 2L))
)

expect_warning(
  expect_equal(get_paths(paths = c("", "")), c("", "")),
  pattern = "'.libPaths()' did not return any non-empty paths",
  strict = TRUE, fixed = TRUE)

expect_warning(
  expect_equal(get_paths(paths = paths_alt), paths_alt),
  pattern = paste0("The currently used R version (4.5.2) is not present as",
                   " directory 'R-4.5.2' or\n'4.5' in any of the retrieved paths"),
  strict = TRUE, fixed = TRUE)


#### Remove objects used in tests ####
rm(current_R, old_R, paths, paths_alt, string_R)
