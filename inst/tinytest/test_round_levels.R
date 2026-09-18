#### Creating values to use in tests ####
num_in <- c(-0.1, 1, -1e4, 1e5) / 7
vals_out_selective <- c(-0.0143, 0.143, -1429, 14286)
vals_out_expanded <- c(-0.014286, 0.14286, -1428.6, 14286)
types_round <- c("selective", "expanded")
types_error <- paste0("'type' should be one of ", paste_quoted(types_round), "!")
x_classes <- c("numeric", "character", "factor")
error_x_class <- paste0(
  "'x_class' should be one of ", paste_quoted(x_classes), "!")
order_classes <- c("numeric", "character")
error_order_class <- paste0(
  "'order_class' should be one of ", paste_quoted(order_classes), "!")


#### Tests ####
for(ind_type_round in seq_along(types_round)) {
  type_round <- types_round[ind_type_round]

  for(ind_x_class in seq_along(x_classes)) {
    x_class <- x_classes[ind_x_class]

    for(ind_order_class in seq_along(order_classes)) {
      order_class <- order_classes[ind_order_class]

      expect_equal(
        round_levels(
          x = switch(x_class,
                     numeric = num_in,
                     character = as.character(num_in),
                     factor = factor(num_in, levels = num_in),
                     stop(error_x_class)),
          level_order = switch(order_class,
                               numeric = num_in,
                               character = as.character(num_in),
                               stop(error_order_class)),
          digits = 3L,
          type = type_round),
        switch(
          type_round,
          selective = factor(x = vals_out_selective, levels = vals_out_selective),
          expanded = factor(x = vals_out_expanded, levels = vals_out_expanded),
          stop(types_error)),
        info = paste0(
          "1", letters[ind_type_round], " ('x': ", x_class, "; 'level_order': ",
          order_class, "; 'x' and 'level_order' in the same order;",
          " rounding type: '", type_round, "')")
      )

      expect_equal(
        round_levels(
          x = switch(x_class,
                     numeric = num_in,
                     character = as.character(num_in),
                     factor = factor(num_in, levels = num_in),
                     stop(error_x_class)),
          level_order = switch(order_class,
                               numeric = num_in[c(2:1, 3:4)],
                               character = as.character(num_in)[c(2:1, 3:4)],
                               stop(error_order_class)),
          digits = 3L,
          type = type_round),
        switch(
          type_round,
          selective = factor(x = vals_out_selective, levels = vals_out_selective[c(2:1, 3:4)]),
          expanded = factor(x = vals_out_expanded, levels = vals_out_expanded[c(2:1, 3:4)]),
          stop(types_error)),
        info = paste0(
          "2", letters[ind_type_round], " ('x': ", x_class, "; 'level_order': ",
          order_class, "; 'x' and 'level_order' in different order;",
          " rounding type: '", type_round, "')")
      )

      expect_warning(
        expect_equal(
          round_levels(
            x = switch(x_class,
                       numeric = num_in,
                       character = as.character(num_in),
                       factor = factor(num_in, levels = num_in),
                       stop(error_x_class)),
            level_order = switch(order_class,
                                 numeric = c(num_in[c(2:1, 4)], 314, 159),
                                 character = c(as.character(num_in)[c(2:1, 4)], 314, 159),
                                 stop(error_order_class)),
            digits = 3L,
            type = type_round),
          switch(type_round,
                 selective = factor(x = vals_out_selective,
                                    levels = vals_out_selective[c(2:1, 4:3)]),
                 expanded = factor(x = vals_out_expanded,
                                   levels = vals_out_expanded[c(2:1, 4:3)]),
                 stop(types_error)),
          info = paste0(
            "3", letters[ind_type_round], " ('x': ", x_class, "; 'level_order': ",
            order_class, "; 'x' and 'level_order' in different order; append",
            " values present in 'x' but missing from 'level_order' as factor",
            " levels, with a warning ; silently drop extraneous levels from",
            " 'level_order'; rounding type: '", type_round, "')")
        ),
        pattern = paste0(
          "Appended levels of 'x' that were not present in 'new_order' to 'new_order':\n",
          paste_quoted(signif_custom(x = num_in, digits = 3L, type = type_round)[3]))
      )

      expect_error(
        round_levels(
          x = switch(x_class,
                     numeric = num_in,
                     character = as.character(num_in),
                     factor = factor(num_in, levels = num_in),
                     stop(error_x_class)),
          level_order = switch(order_class,
                               numeric = num_in[c(2:1, 3:4)],
                               character = as.character(num_in)[c(2:1, 3:4)],
                               stop(error_order_class)),
          digits = 1:4,
          type = type_round),
        pattern = "is_number(digits) is not TRUE", fixed = TRUE,
        info = paste0(
          "3b", letters[ind_type_round], " ('x': ", x_class, "; 'level_order': ",
          order_class, "; 'x' and 'level_order' in different order; error if",
          " length 'digits' > 1L; rounding type: '", type_round, "')")
      )
    }

    # sort(as.character(num_in)) sorts in lexicographical order, which is not
    # the correct numerical order: this test also checks that numeric values are
    # used to sort on for non-numeric input to 'x'.
    expect_equal(
      round_levels(
        x = switch(x_class,
                   numeric = num_in,
                   character = as.character(num_in),
                   factor = factor(num_in, levels = num_in),
                   stop(error_x_class)),
        level_order = NULL,
        digits = 3L,
        type = type_round),
      switch(type_round,
             selective = factor(x = vals_out_selective,
                                levels = sort(vals_out_selective)),
             expanded = factor(x = vals_out_expanded,
                               levels = sort(vals_out_expanded))),
      info = paste0(
        "4", letters[ind_type_round], " ('x': ", x_class, "; 'level_order': NULL",
        "; 'x' sorted in increasing order used to define levels, without",
        " changing order of 'x' itself; rounding type: '", type_round, "')")
    )
  }

  for(order_class in order_classes) {
    expect_error(
      round_levels(
        x = NULL,
        level_order = switch(order_class,
                             numeric = num_in,
                             character = as.character(num_in),
                             stop(error_order_class)),
        digits = 3L,
        type = type_round), pattern = "length(x) > 0L is not TRUE", fixed = TRUE,
      info = paste0("5", letters[ind_type_round], "('x': NULL; 'level_order': ",
                    order_class, "; rounding type: '", type_round, "')")
    )
  }
}

# 'digits' is rounded to the nearest integer in 1 - 22
expect_equal(round_levels(x = num_in, digits = -3),
             round_levels(x = num_in, digits = 1)
)


#### Input that should give an error ####
for(x in list(data.frame(num_in), matrix(num_in))) {
  expect_error(
    round_levels(x = x, level_order = num_in),
    pattern = "is.null(dim(x)) is not TRUE", fixed = TRUE,
    info = "6a ('x' should be a vector, not a dataframe or matrix)")
}

for(x in list(list(num_in), as.list(num_in))) {
  expect_error(
    round_levels(x = x, level_order = num_in),
    pattern = "!is.list(x) is not TRUE", fixed = TRUE,
    info = "6b ('x' should be a vector, not a list)")
}

expect_error(
  round_levels(x = num_in, level_order = as.factor(num_in)),
  pattern = "is.null(level_order) || is.vector(level_order) is not TRUE",
  fixed = TRUE,
  info = "7 ('level_order' should be a vector, not a factor)")

expect_error(
  round_levels(x = num_in, digits = "a"),
  pattern = "checkinput::is_number(digits) is not TRUE", fixed = TRUE,
  info = "8 ('digits' should be numeric)")

expect_error(
  round_levels(x = num_in, type = "abc"),
  pattern = "'arg' should be one of",
  info = "9 ('digits' should be numeric)")


#### Cleaning up ####
rm(error_order_class, types_error, error_x_class, ind_order_class, ind_type_round,
   ind_x_class, num_in, order_class, order_classes, type_round, types_round,
   vals_out_expanded, vals_out_selective, x, x_class, x_classes)
