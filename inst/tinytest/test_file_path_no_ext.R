#### Create objects to use in tests ####
invalid_names <- c("CON", "PRN", "AUX", "NUL", paste0("COM", 1:9),
                   paste0("LPT", 1:9))
illegal_char <- c('"', "*", "/", ":", "?", "\\", "|", "<", ">")


#### Test the examples ####
if(getRversion() < "4.6.0") {
  expect_silent(
    expect_identical(
      tools::file_path_sans_ext("ab..txt"),
      "ab..txt"))
} else {
  expect_silent(
    expect_identical(
      tools::file_path_sans_ext("ab..txt"),
      "ab."))
}

expect_silent(
  expect_identical(
    tools::file_ext("ab..txt"),
    "txt")
)

# Prior to R 4.6.0, this produced the nonsense-result "ab..txt.txt" instead of
# re-creating 'filename' because tools::file_path_sans_ext() did not remove the
# extension if the filename ended in a dot.
if(getRversion() < "4.6.0") {
  expect_identical(
    paste0(tools::file_path_sans_ext("ab..txt"), ".", tools::file_ext("ab..txt")),
    "ab..txt.txt")
} else {
  expect_identical(
    paste0(tools::file_path_sans_ext("ab..txt"), ".", tools::file_ext("ab..txt")),
    "ab..txt")
}

# Correct result
expect_silent(
  expect_identical(
    paste0(file_path_no_ext("ab..txt"), ".", tools::file_ext("ab..txt")),
    "ab..txt")
)

expect_silent(
  expect_identical(
    paste0(file_path_no_ext("ab..txt"), ".", file_path_ext("ab..txt")),
    "ab..txt")
)


#### Tests ####
##### Without extension #####
for(x in c(".", "..", "a", "abcd", "abcd ", "abcd.", "COM", "LPT",
           paste0("ab", illegal_char, "cd"), invalid_names, "abc.tx#")) {
  expect_identical(file_path_no_ext(x = x, compression = FALSE), x)
  expect_identical(file_path_ext(x = x, compression = FALSE), "")
  expect_identical(file_path_no_ext(x = x, compression = TRUE), x)
  expect_identical(file_path_ext(x = x, compression = TRUE), "")
}

##### With file extension #####
x_ext <- c(".abcd.txt", "a.txt",
           paste0("ab", illegal_char, "cd.txt"), "ab.cd.txt", "abcd .txt",
           "abcd..txt", "abcd.txt", "COM.txt", "COM0.txt", "LPT.txt",
           "LPT0.txt")
x_ext_path_out <- c(".abcd", "a", paste0("ab", illegal_char, "cd"),
                    "ab.cd", "abcd ", "abcd.", "abcd", "COM", "COM0", "LPT",
                    "LPT0")
x_ext_ext_out <- rep.int("txt", 19L)

for(i in seq_along(x_ext)) {
  x <- x_ext[i]
  expect_identical(file_path_no_ext(x = x, compression = FALSE), x_ext_path_out[i])
  expect_identical(file_path_ext(x = x, compression = FALSE), x_ext_ext_out[i])
  expect_identical(file_path_no_ext(x = x, compression = TRUE), x_ext_path_out[i])
  expect_identical(file_path_ext(x = x, compression = TRUE), x_ext_ext_out[i])
}

##### With compression extension #####
x_compr <- c("a.gz", "abcd.bz2", "abcd.gz", "abcd.xz")
x_compr_path_out <- c("a", "abcd", "abcd", "abcd")
x_compr_ext_out <- c("gz", "bz2", "gz", "xz")

for(i in seq_along(x_compr)) {
  x <- x_compr[i]
  expect_identical(file_path_no_ext(x = x, compression = FALSE), x_compr_path_out[i])
  expect_identical(file_path_ext(x = x, compression = FALSE), x_compr_ext_out[i])
  expect_identical(file_path_no_ext(x = x, compression = TRUE), x_compr_path_out[i])
  expect_identical(file_path_ext(x = x, compression = TRUE), "")
}

##### With both extensions #####
x_both <- c("a.txt.gz", "abcd.txt.bz2", "abcd.txt.gz", "abcd.txt.xz")
x_both_path_out_no_compr <- c("a.txt", "abcd.txt", "abcd.txt", "abcd.txt")
x_both_path_out_with_compr <- c("a", "abcd", "abcd", "abcd")
x_both_ext_out_no_compr <- c("gz", "bz2", "gz", "xz")

for(i in seq_along(x_both)) {
  x <- x_both[i]
  expect_identical(file_path_no_ext(x = x, compression = FALSE), x_both_path_out_no_compr[i])
  expect_identical(file_path_ext(x = x, compression = FALSE), x_both_ext_out_no_compr[i])
  expect_identical(file_path_no_ext(x = x, compression = TRUE), x_both_path_out_with_compr[i])
  expect_identical(file_path_ext(x = x, compression = TRUE), "txt")
}

##### Spurious #####
warning("test_file_path_no_ext.R: Various extensions are NOT handled",
        " symmetrically by file_path_no_ext() and file_path_ext()!")
x_ext <- c("..txt", ".txt", ".gz", "..gz", ".txt.gz", "...gz")
x_ext_path_out <- c(".", "", "", ".", ".txt", "..")
x_ext_path_out_compr <- x_ext_path_out
x_ext_path_out_compr[5] <- ""
x_ext_ext_out_no_compr <- rep.int("", 6L)
x_ext_ext_out_no_compr[5] <- "gz"

for(i in seq_along(x_ext)) {
  x <- x_ext[i]
  expect_identical(file_path_no_ext(x = x, compression = FALSE), x_ext_path_out[i])
  expect_identical(file_path_ext(x = x, compression = FALSE), x_ext_ext_out_no_compr[i])
  expect_identical(file_path_no_ext(x = x, compression = TRUE), x_ext_path_out_compr[i])
  expect_identical(file_path_ext(x = x, compression = TRUE), "")
}


#### Remove objects used in tests ####
rm(i, illegal_char, invalid_names, x, x_both, x_both_ext_out_no_compr,
   x_both_path_out_no_compr, x_both_path_out_with_compr, x_compr,
   x_compr_ext_out, x_compr_path_out, x_ext, x_ext_ext_out,
   x_ext_ext_out_no_compr, x_ext_path_out, x_ext_path_out_compr)
