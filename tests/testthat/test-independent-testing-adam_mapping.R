x <- adam_mapping(
  name = "apat",
  id = "USUBJID",
  group = "TRT01A",
  subset = TRTFL == "Y",
  label = "All Participants as Treated"
)
x

test_that("output x is a adam_mapping class object", {
  expect_equal(class(x), "adam_mapping")
})

test_that(" name, id, group, var, subset, label is the default vector of column name of the list x", {
  expect_equal(names(x), c("name", "id", "group", "var", "subset", "label"))
})

test_that("name, id, group, subset, label would be mapped to their input or default value accordingly", {
  expect_equal(c(x$name, x$id, x$group, x$subset, x$label), c("apat", "USUBJID", "TRT01A", quote(TRTFL == "Y"), "All Participants as Treated"))

  def <- adam_mapping(name = "y")

  expect_equal(def$name, "y")
  expect_null(c(def$id, def$group, def$subset, def$label))
})
