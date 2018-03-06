context("leaflet")

quakes <- eq_location_clean()

test_that("eq_map creates a leaflet", {
  map <- eq_map(quakes, annot_col = "DATE")
  expect_equal(class(map), c("leaflet", "htmlwidget"))
})


test_that("eq_create_label returns a proper HTML label", {
  label <- eq_create_label(quakes)
  expect_equal(label[1], "<b>Location name:</b>Bab-A-Daraa,Al-Karak<br /> <b>Magnitude:</b> 7.3<br />  ")
})
