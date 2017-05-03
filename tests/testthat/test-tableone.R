test_that(
  "Basic tables work",
  {
    mtcars_6_8 <- subset(mtcars, cyl %in% c(6, 8))
    expect_equal(
      table_one(mtcars_6_8, mpg),
      structure(
        c("mpg", "", "16.4 (15.0 - 19.2)"),
        .Dim = c(1L, 3L),
        .Dimnames = list(NULL, c("Variable", "Level", "Value"))
      )
    )
  })
