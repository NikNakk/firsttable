test_that(
  "pretty p works",
  {
    expect_equal(
      lapply(
        c("<", "E", "x10", "plotmath"),
        pretty_p,
        p = c(0.001, 0.0001),
        p_digits = 3
        ),
      list(
        c("0.001", "<0.001"),
        c("0.001", "1.0E-04"),
        c("0.001", "1.0x10^-4"),
        c("0.001", "1.0%*% 10^-4")
      )
    )
  }
)
