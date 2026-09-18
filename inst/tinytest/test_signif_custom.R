#### signif_custom ####
x1 <- c(-0.1, 1) / 7
x2 <- c(-1e4, 1e5) / 7
x12 <- c(x1, x2)
x_exp <- 10^c(-2, -4, 2, 4) / 7
x_exp <- c(-1 * x_exp, x_exp)

x1_df <- as.data.frame(x1)
x2_df <- as.data.frame(x2)
x12_df <- as.data.frame(x12)
x_exp_df <- as.data.frame(x_exp)
x1_df_cols <- as.data.frame(rbind(x1))
x12_df_cols <- as.data.frame(cbind(x1_df, x2_df))

x1_mat <- as.matrix(x1)
x2_mat <- as.matrix(x2)
x12_mat <- as.matrix(x12)
x_exp_mat <- as.matrix(x_exp_df)
x1_mat_cols <- rbind(x1)
x12_mat_cols <- as.matrix(cbind(x1_df, x2_df))

x_df <- data.frame(a = 1:3, b = letters[11:13], c = c(1e5, 1e3, 1) / 7, d = pi)
expect_equal(
  signif_custom(x_df, type = "selective"),
  data.frame(a = 1:3, b = letters[11:13], c = c(14286, 143, 0.143),
             d = rep(3.14, 3L))
)
expect_equal(
  signif_custom(x_df, type = "expanded"),
  data.frame(a = 1:3, b = letters[11:13], c = c(14286, 142.86, 0.14286),
             d = rep(3.14, 3L))
)

for(type in c("selective", "expanded")) {
  expect_silent(
    expect_equal(
      signif_custom(x = x1, digits = 3, type = type),
      c(-0.0143, 0.143),
      info = paste0("1 (abs(x) < 10^digits, type '", type, "')")
    )
  )

  expect_equal(
    signif_custom(x = x1_df, digits = 3, type = type),
    data.frame(x1 = c(-0.0143, 0.143)),
    info = paste0("1_df (abs(x) < 10^digits, type '", type, "')")
  )

  expect_x1_df_cols <- data.frame(rbind(x1 = c(V1 = -0.0143, V2 = 0.143)))
  expect_equal(
    signif_custom(x = x1_df_cols, digits = 3, type = type),
    expect_x1_df_cols,
    info = paste0("1_df_cols (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x1_mat, digits = 3, type = type),
    unname(as.matrix(data.frame(c(-0.0143, 0.143)))),

    info = paste0("1_mat (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x1_mat_cols, digits = 3, type = type),
    t(as.matrix(data.frame(x1 = c(-0.0143, 0.143)))),
    info = paste0("1_mat_cols (abs(x) < 10^digits, type '", type, "')")
  )

  if(type == "selective") {
    expect_x2 <- c(-1429, 14286)
  } else {
    expect_x2 <- c(-1428.6, 14286)
  }

  expect_equal(
    signif_custom(x = x2, digits = 3, type = type),
    expect_x2,
    info = paste0("2 (abs(x) > 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x2_df, digits = 3, type = type),
    data.frame(x2 = expect_x2),
    info = paste0("2_df (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x2_mat, digits = 3, type = type),
    unname(as.matrix(data.frame(expect_x2))),
    info = paste0("2_mat (abs(x) < 10^digits, type '", type, "')")
  )

  if(type == "selective") {
    expect_x12 <- c(-0.0143, 0.143, -1429, 14286)
  } else {
    expect_x12 <- c(-0.014286, 0.14286, -1428.6, 14286)
  }

  expect_equal(
    signif_custom(x = x12, digits = 3, type = type),
    expect_x12,
    info = paste0("3 (some abs(x) > 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x12_df, digits = 3, type = type),
    data.frame(x12 = expect_x12),
    info = paste0("3_df (abs(x) < 10^digits, type '", type, "')")
  )

  expect_x12_df_cols <- data.frame(x1 = t(unname(expect_x1_df_cols)),
                                   x2 = unname(cbind(expect_x2)))
  expect_equal(
    signif_custom(x = x12_df_cols, digits = 3, type = type),
    expect_x12_df_cols,
    info = paste0("3_df_cols (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x12_mat, digits = 3, type = type),
    as.matrix(expect_x12),
    info = paste0("3_mat (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x12_mat_cols, digits = 3, type = type),
    as.matrix(expect_x12_df_cols),
    info = paste0("3_mat_cols (abs(x) < 10^digits, type '", type, "')")
  )

  if(type == "selective") {
    expect_exp <- c(-0.00143, -1.43e-05, -14.3, -1429,
                    0.00143,  1.43e-05,  14.3,  1429)
  } else {
    expect_exp <- signif(x = x_exp, digits = 4)
  }

  expect_equal(
    signif_custom(x = x_exp, digits = 3, type = type),
    expect_exp,
    info = paste0("4 (some abs(x) larger than 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x_exp_df, digits = 3, type = type),
    data.frame(x_exp = expect_exp),
    info = paste0("4_df (abs(x) < 10^digits, type '", type, "')")
  )

  expect_exp_mat <- as.matrix(x = expect_exp)
  colnames(expect_exp_mat) <- "x_exp"
  expect_equal(
    signif_custom(x = x_exp_mat, digits = 3, type = type),
    expect_exp_mat,
    info = paste0("4_mat (abs(x) < 10^digits, type '", type, "')")
  )

  expect_error(
    signif_custom(x = NULL, type = type),
    pattern = "is.numeric(x) is not TRUE", fixed = TRUE,
    info = paste0("5a (error for zero-length 'x', type '", type, "')")
  )

  expect_error(
    signif_custom(x = "a", type = type),
    pattern = "is.numeric(x) is not TRUE", fixed = TRUE,
    info = paste0("5b (error for non-numeric 'x', type '", type, "')")
  )

  expect_error(
    signif_custom(x = as.factor(x1), type = type),
    pattern = "'signif_custom()' does not handle factors", fixed = TRUE,
    info = paste0("5c (specific error for factor 'x', type '", type, "')")
  )

  expect_error(
    signif_custom(x = 0.01 / 7, digits = integer(0), type = type),
    pattern = "is_number(digits) is not TRUE", fixed = TRUE,
    info = paste0("6 (error for zero-length 'digits', type '", type, "')")
  )

  if(type == "selective") {
    expect_t7 <- c(-1e-3, -1e-5, -14, -1429, 1e-3, 1e-5, 14, 1429)
  } else {
    expect_t7 <- signif(x = x_exp, digits = 4)
  }

  expect_equal(
    signif_custom(x = x_exp, digits = -3, type = type),
    expect_t7,
    info = paste0("7 (negative values for 'digits' are set to 0, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = x_exp_df, digits = -3, type = type),
    data.frame(x_exp = expect_t7),
    info = paste0("7_df (abs(x) < 10^digits, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = c(Inf, pi), digits = 3, type = type),
    c(Inf, 3.14),
    info = paste0("Inf and numeric values, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = c(Inf, NA_real_, pi), digits = 3, type = type),
    c(Inf, NA_real_, 3.14),
    info = paste0("Inf, NA, and numeric values, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = c(NA_real_, NA_real_), digits = 3, type = type),
    c(NA_real_, NA_real_),
    info = paste0("Only NAs, type '", type, "')")
  )

  expect_equal(
    signif_custom(x = c(Inf, -Inf), digits = 3, type = type),
    c(Inf, -Inf),
    info = paste0("Only Infs, type '", type, "')")
  )
}

rm(expect_exp, expect_exp_mat, expect_t7, expect_x2, expect_x12_df_cols,
   expect_x12, expect_x1_df_cols, type, x1, x1_df, x1_df_cols,
   x1_mat, x1_mat_cols, x2, x2_df, x2_mat, x12, x12_df, x12_df_cols, x12_mat,
   x12_mat_cols, x_df, x_exp, x_exp_df, x_exp_mat)
