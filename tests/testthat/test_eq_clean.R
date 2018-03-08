quakes_wrangled <- eq_location_clean()

test_that("eq_location_clean returns a tbl_df, tbl, and data.frame", {
  expect_equal(class(quakes_wrangled),"data.frame")
})

test_that("quakes_wrangled contains a DATE column", {
  expect_true("DATE" %in% names(quakes_wrangled))
})

test_that("eq_location_clean converts LATITUDE and LONGITUDE to numeric", {
  expect_equal(class(quakes_wrangled$LATITUDE), "numeric")
  expect_equal(class(quakes_wrangled$LONGITUDE), "numeric")
})



test_that("eq_clean_data converts LOCATION", {
  expect_equal(quakes_wrangled[1, ]$LOCATION_NAME, "Bab-A-Daraa,Al-Karak")
})
