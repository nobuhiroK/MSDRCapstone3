context("GeomTimelene")

quakes <- eq_location_clean()

test_that("The layer is a GeomTimeline", {
  g <- ggplot2::ggplot(quakes, aes(x = DATE,
                     y = COUNTRY,
                     colour = as.numeric(TOTAL_DEATHS),
                     size = as.numeric(EQ_PRIMARY)))
  g <- g + geom_timeline()
  expect_is(g$layers[[1]]$geom, "GeomTimeline")
})

test_that("The layer is a GeomTimelineLabel", {
  g <- ggplot2::ggplot(quakes, aes(x = DATE, y = COUNTRY,
                                   size = EQ_PRIMARY, color = TOTAL_DEATHS))
  g <- g + geom_timeline()
  g <- g + geom_timeline_label(aes(magnitude = as.numeric(EQ_PRIMARY),
                                   label = LOCATION_NAME, n_max = 5))
  expect_is(g$layers[[2]]$geom, "GeomTimelineLabel")
})

test_that("geom_timeline_label has correct label", {
  g <- ggplot2::ggplot(quakes, aes(x = DATE, y = COUNTRY,
                                   size = EQ_PRIMARY, color = TOTAL_DEATHS))
  g <- g + geom_timeline()
  g <- g + geom_timeline_label(aes(magnitude = as.numeric(EQ_PRIMARY),
                                   label = LOCATION_NAME, n_max = 5))
  expect_equal(g$layers[[2]]$mapping$label, as.name("LOCATION_NAME"))
})
