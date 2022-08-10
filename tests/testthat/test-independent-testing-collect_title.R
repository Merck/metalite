test_that("output is is a string vector", {
  expect_equal(collect_title(meta_dummy(), "apat", "wk12", "ser", "ae_summary"), 
               c("Summary of Adverse Events", "Weeks 0 to 12", "All Participants as Treated"))
  expect_equal(collect_title(meta_dummy(), "apat", "wk12", "ser", "ae_forest"), 
               c("Weeks 0 to 12", "All Participants as Treated"))
  
})
