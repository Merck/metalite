x <- default_apply(adam_mapping(name = "apat"))

test_that("defalut values for name=apata", {
  expect_equal(
    c(x$name, x$id, x$group, x$var, x$subset, x$label),
    c("apat", "USUBJID", NULL, NULL, NULL, "All Participants as Treated")
  )
})

y <- default_apply(adam_mapping(name = "apr"))

test_that("defalut values for name=apr", {
  expect_equal(
    c(y$name, y$id, y$group, y$var, y$subset, y$label),
    c("apr", "USUBJID", NULL, NULL, NULL, "All Participants Randomized")
  )
})

z <- default_apply(adam_mapping(name = "apt"))

test_that("defalut values for name=apt", {
  expect_equal(
    c(z$name, z$id, z$group, z$var, z$subset, z$label),
    c("apt", "USUBJID", NULL, NULL, NULL, "All Participants Treated")
  )
})

a <- default_apply(adam_mapping(name = "asat"))

test_that("defalut values for name=asat", {
  expect_equal(
    c(a$name, a$id, a$group, a$var, a$subset, a$label),
    c("asat", "USUBJID", NULL, NULL, NULL, "All Subjects as Treated")
  )
})

b <- default_apply(adam_mapping(name = "asr"))

test_that("defalut values for name=asr", {
  expect_equal(
    c(b$name, b$id, b$group, b$var, b$subset, b$label),
    c("asr", "USUBJID", NULL, NULL, NULL, "All Subjects Randomized")
  )
})

c <- default_apply(adam_mapping(name = "ast"))

test_that("defalut values for name=ast", {
  expect_equal(
    c(c$name, c$id, c$group, c$var, c$subset, c$label),
    c("ast", "USUBJID", NULL, NULL, NULL, "All Subjects Treated")
  )
})
