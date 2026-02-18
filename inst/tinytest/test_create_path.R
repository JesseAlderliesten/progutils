warning("test_create_path.R is not yet complete!")

#### Test the examples ####
warning("Test examples of create_path()!")
# my_tempdir <- tempdir()

# Cleaning up
# unlink(c(res_dir_one, dirname(res_dir_two)), recursive = TRUE)
# rm(my_tempdir, res_dir_one, res_dir_one_v2, res_dir_one_v3, res_dir_two)


#### Tests ####
my_tempdir <- tempdir()
dir_out_p1 <- normalizePath(path = my_tempdir, winslash = "/", mustWork = FALSE)

expect_error(create_path(filename = "abcd", dir = my_tempdir),
             pattern = "Filename 'abcd' should contain a file extension",
             fixed = TRUE)
expect_error(create_path(filename = "abc.", dir = my_tempdir),
             pattern = "Filename 'abc.' should contain a file extension",
             fixed = TRUE)
expect_error(create_path(filename = "ab.c#", dir = my_tempdir),
             pattern = "Filename 'ab.c#' should contain a file extension",
             fixed = TRUE)

filenm_in <- c("a/a.txt", "b\\b.txt")
filenm_out <- c("a_a.txt", "b_b.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_warning(
    expect_equal(
      create_path(filename = filenm_in[ind_filenm], format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
      paste0(dir_out_p1, "/", filenm_out[ind_filenm])
    ),
    pattern = paste0("Replaced (back)slashes in filename '",
                     filenm_in[ind_filenm], "' with underscores"),
    strict = TRUE, fixed = TRUE)
}

# Programming notes:
# - File names that end in a dot are handled incorrectly by create_path():
#   tools::file_path_sans_ext() does not recognise the extension of file names
#   that end in a dot, whereas tools::file_ext() does recognise such extension.
#   Therefore, create_path("ff..txt") produces the nonsense result ending in
#   "ff__txt.txt" instead of ending in "ff_.txt".
filenm_in <- c("c#c.txt", "d d.txt", "e.e.txt")
filenm_out <- c("c_c.txt", "d_d.txt", "e_e.txt")
for(ind_filenm in seq_along(filenm_in)) {
  expect_warning(
    expect_equal(
      create_path(filename = filenm_in[ind_filenm], format_stamp = "",
                  dir = my_tempdir, add_date = FALSE),
      paste0(dir_out_p1, "/", filenm_out[ind_filenm])
    ),
    pattern = paste0("Replaced non-alphanumeric characters in filename '",
                     filenm_in[ind_filenm], "' with underscores"),
    strict = TRUE, fixed = TRUE)
}

expect_silent(
  expect_equal(
    create_path(filename = "g_g.txt", format_stamp = "",
                dir = my_tempdir, add_date = FALSE),
    paste0(dir_out_p1, "/g_g.txt"))
)




# create_path(filename = "test1a.txt", format_stamp = "", add_date = TRUE)
# create_path(filename = "test1b.txt", format_stamp = "%H_%M", add_date = TRUE)
# create_path(filename = "test1c.txt", format_stamp = "%H_%M_%OS3", add_date = TRUE)
# create_path(filename = "test2a.html", format_stamp = "", add_date = FALSE)
# create_path(filename = "test2b.html", format_stamp = "%H_%M", add_date = FALSE)
# create_path(filename = "test2c.html", format_stamp = "%H_%M_%OS3", add_date = FALSE)

# Filenames that already exist, filenames that are directories
# ...


# my_tempfile <- file.path(tempdir(), "test_df.csv")
# # Write csv-file, modified from example in help(write.table)
# write.table(x = data.frame(a = "a", b = pi), file = my_tempfile)
#
#
# dir <- file.path(my_tempdir, "temp_subdirF_dateF")
# expected_path <- dir


# # 5 Checks on input to 'dir'
# for(dir in list(3, "", character(0), NULL, c("temp_p1", "temp_p2"))) {
#   expect_error(
#     create_dir(dir = dir),
#     pattern = "is_character(dir) is not TRUE", fixed = TRUE)
# }
#
# for(dir in list(paste0(my_tempdir, "./"), paste0(my_tempdir, "temp_p1/"))) {
#   expect_error(
#     create_dir(dir = dir),
#     pattern = "'dir' should not end with '/'", fixed = TRUE)
# }
#
# for(dir in list(paste0(my_tempdir, ".\\"), paste0(my_tempdir, "temp_p1\\"))) {
#   expect_error(
#     create_dir(dir = dir),
#     pattern = "'dir' should not end with '\\'", fixed = TRUE)
# }
#
# for(dir in list(paste0(my_tempdir, ".."), paste0(my_tempdir, "temp_p1."))) {
#   expect_error(
#     create_dir(dir = dir),
#     pattern = "'dir' should not end with '.'", fixed = TRUE)
# }
#
# for(dir in list(paste0(my_tempdir, ". "), paste0(my_tempdir, "temp_p1 "))) {
#   expect_error(
#     create_dir(dir = dir),
#     pattern = "'dir' should not end with ' ' (i.e., a space)", fixed = TRUE)
# }
#
# # Check if illegal characters are recognised
# expect_false(grepl(pattern = "[<]", x = "abc"))
# expect_false(grepl(pattern = "[>]", x = "abc"))
# expect_false(grepl(pattern = '["]', x = 'abc'))
# expect_false(grepl(pattern = "[|]", x = "abc"))
# expect_false(grepl(pattern = "[?]", x = "abc"))
# expect_false(grepl(pattern = "[*]", x = "abc"))
#
# expect_true(grepl(pattern = "[<]", x = "ab<c"))
# expect_true(grepl(pattern = "[>]", x = "ab>c"))
# expect_true(grepl(pattern = '["]', x = 'ab"c'))
# expect_true(grepl(pattern = "[|]", x = "ab|c"))
# expect_true(grepl(pattern = "[?]", x = "ab?c"))
# expect_true(grepl(pattern = "[*]", x = "ab*c"))
#
# expect_true(grepl(pattern = '[<>"|?*]', x = "ab<c"))
# expect_true(grepl(pattern = '[<>"|?*]', x = "ab>c"))
# expect_true(grepl(pattern = '[<>"|?*]', x = 'ab"c'))
# expect_true(grepl(pattern = '[<>"|?*]', x = "ab|c"))
# expect_true(grepl(pattern = '[<>"|?*]', x = "ab?c"))
# expect_true(grepl(pattern = '[<>"|?*]', x = "ab*c"))
#
# # 'dir' points to a file instead of a directory
# expect_warning(
#   expect_equal(create_dir(dir = my_tempfile, add_date = FALSE),
#                normalizePath(getwd())),
#   pattern = paste0("failed! Returning\nthe working directory"))
#
# # 6 Checks on input to 'add_date'
# for(add_date in list(3, NA)) {
#   expect_error(
#     create_dir(dir = my_tempdir, add_date = add_date),
#     pattern = "is_logical(add_date) is not TRUE", fixed = TRUE)
# }
#
#
# #### Delete the created temporary files ####
# unlink(my_tempfile, recursive = TRUE)
#
#
# #### Remove objects used in tests ####
# rm(add_date, dir, expected_path, my_tempdir, my_tempfile)
