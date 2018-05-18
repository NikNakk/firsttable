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
test_that(
  "Survival columns work",
  {
    expect_equal(
      first_table(lung,
                  .column_variable = Surv(time, status),
                  .options = list(include_n = TRUE),
                  ECOG = factor(ph.ecog),
                  `Meal calories` = first_table_row(meal.cal, row_digits = 2)
                  ),
      structure(
        c(
          "ECOG",
          "",
          "",
          "",
          "Meal calories",
          "227",
          "",
          "",
          "",
          "181",
          "0",
          "1",
          "2",
          "3",
          "",
          "Reference",
          "1.4 (1.0 - 2.1)",
          "2.5 (1.6 - 3.9)",
          "9.1 (1.2 - 67.9)",
          "1.00 (1.00 - 1.00)",
          "",
          "0.063",
          "<0.001",
          "0.031",
          "0.593"
        ),
        .Dim = c(5L, 5L),
        .Dimnames = list(NULL, c(
          "Variable", "n", "Level", "Hazard ratio (95% CI)",
          "p"
        ))
      )
    )
  })
test_that(
  "Warnings for deprecated options",
  {
    expect_warning(
      first_table(mtcars,
                  column_variable = cyl,
                  mpg
      ),
      regexp = "Column variable should now be specified"
    )
    expect_warning(
      first_table(mtcars,
                  include_n = TRUE,
                  mpg
      ),
      regexp = "Options should now be specified"
    )
  })
test_that(
  "Tests of kruskal_row and other options",
  {
    expect_equal(
      first_table(
        mtcars,
        .column_variable = cyl,
        .options = list(include_n_per_col = TRUE),
        fisher_row(factor(am), include_reference = FALSE, reference_level = NULL),
        mpg
        ),
      structure(c("n", "factor(am)", "mpg", "", "1", "", "11", "8 (72.7%)",
                  "26.0 (22.8 - 30.4)", "7", "3 (42.9%)", "19.7 (18.6 - 21.0)",
                  "14", "2 (14.3%)", "15.2 (14.4 - 16.2)", "", "0.009", "<0.001"
      ), .Dim = c(3L, 6L), .Dimnames = list(NULL, c("Variable", "Level",
                                                    "4", "6", "8", "p")))
    )
    expect_equal(
      first_table(
        mtcars,
        .column_variable = cyl,
        .options = first_table_options(include_p = FALSE),
        mpg
      ),
      structure(c("mpg", "", "26.0 (22.8 - 30.4)", "19.7 (18.6 - 21.0)",
                  "15.2 (14.4 - 16.2)"), .Dim = c(1L, 5L), .Dimnames = list(NULL,
                  c("Variable", "Level", "4", "6", "8")))
    )
})

test_that(
  "Test of parametric_row",
  {
    expect_equal(
      first_table(
        mtcars,
        .column_variable = am,
        .options = first_table_options(default_non_parametric = FALSE),
        mpg
      ),
      structure(c("mpg", "", "17.1 (3.8)", "24.4 (6.2)", "0.001"),
                .Dim = c(1L, 5L),
                .Dimnames = list(NULL, c("Variable", "Level", "0", "1", "p")))
    )
    expect_equal(
      first_table(
        mtcars,
        .options = first_table_options(include_p = FALSE),
        parametric_row(mpg)
      ),
      structure(c("mpg", "", "20.1 (6.0)"), .Dim = c(1L, 3L), .Dimnames = list(
        NULL, c("Variable", "Level", "Value")))
    )
  }
)
