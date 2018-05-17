test_that(
  "Basic tables work",
  {
    mtcars_6_8 <- subset(mtcars, cyl %in% c(6, 8))
    mtcars_6_8 <- mtcars_6_8[!duplicated(mtcars_6_8$mpg), ]
    expect_equal(
      first_table(mtcars_6_8, mpg, factor(gear), am == 0),
      structure(
        c(
          "mpg",
          "factor(gear)",
          "",
          "",
          "am == 0",
          "",
          "3",
          "4",
          "5",
          "TRUE",
          "16.4 (15.0 - 18.7)",
          "11 (64.7%)",
          "3 (17.6%)",
          "3 (17.6%)",
          "13 (76.5%)"
        ),
        .Dim = c(5L, 3L),
        .Dimnames = list(NULL, c("Variable", "Level", "Value"))
      )
    )

    expect_equal(
      first_table(mtcars_6_8,
                .column_variable = cyl,
                "Miles per gallon" = wilcox_row(mpg, row_digits = 2),
                "Transmission" = fisher_row(am, reference_level = 0)),
      structure(
        c(
          "Miles per gallon",
          "Transmission",
          "",
          "1",
          "19.45 (18.38 - 20.68)",
          "2 (33.3%)",
          "15.20 (14.50 - 16.10)",
          "2 (18.2%)",
          "<0.001",
          "0.584"
        ),
        .Dim = c(2L, 5L),
        .Dimnames = list(NULL, c("Variable", "Level", "6", "8", "p"))
      )
    )

    expect_equal(
      first_table(mtcars_6_8,
                .column_variable = cyl,
                .options = list(include_n = TRUE),
                "MPG 3 gears" = wilcox_row(mpg, data_filter = gear == 3)),
      structure(
        c(
          "MPG 3 gears",
          "11",
          "",
          "19.8 (18.9 - 20.6)",
          "15.2 (14.3 - 16.4)",
          "0.073"
        ),
        .Dim = c(1L, 6L),
        .Dimnames = list(NULL, c("Variable", "n", "Level", "6", "8", "p"))
      )
    )
  })
