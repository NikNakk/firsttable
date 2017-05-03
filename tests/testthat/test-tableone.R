test_that(
  "Basic tables work",
  {
    mtcars_6_8 <- subset(mtcars, cyl %in% c(6, 8))
    expect_equal(
      table_one(mtcars_6_8, mpg, factor(gear), am == 0),
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
          "16.4 (15.0 - 19.2)",
          "14 (66.7%)",
          "4 (19.0%)",
          "3 (14.3%)",
          "16 (76.2%)"
        ),
        .Dim = c(5L, 3L),
        .Dimnames = list(NULL, c("Variable", "Level", "Value"))
      )
    )

    expect_equal(
      table_one(mtcars_6_8,
                column_variable = cyl,
                "Miles per gallon" = wilcox_row(mpg, row_digits = 2),
                "Transmission" = fisher_row(am, reference_level = 0)),
      structure(
        c(
          "Miles per gallon",
          "Transmission",
          "",
          "1",
          "19.70 (18.65 - 21.00)",
          "3 (42.9%)",
          "15.20 (14.40 - 16.25)",
          "2 (14.3%)",
          "0.001",
          "0.280"
        ),
        .Dim = c(2L, 5L),
        .Dimnames = list(NULL, c("Variable", "Level", "6", "8", "p"))
      )
    )
  })
