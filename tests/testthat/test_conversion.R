library(testthat)
library(corduroy)

cases <- c(
  dd1 = "23",
  dd2 = "23°",
  dd3 = "23.1",
  dd4 = "23,1",
  dd5 = "23,1°",
  dd6 = "N23",
  dd7 = "N23.1",
  dd8 = "N23,1",
  dd9 = "N23,1°",
  dd10 = "23,1°N",
  dd11 = "23.12°N",
  dd12 = "23,12N",
  dd13 = "-23,12° N",
  dd14 = "N -23",

  dms1 = "23°30'30''N",
  dms2 = "N 23°30'30\"",
  dms3 = "23°30`30``",
  dms4 = "23°30`30``",
  dms5 = "23°30`30′′",
  dms6 = " 23 ° 30´ 30′′ N",
  dms7 = " 23 ° 30´ 30.521′′ N",
  dms8 = "N 23° 30' 30.521\"",
  dms9 = "N -23° 30' 30.521\"",

  dm1 = "23°12'",
  dm2 = "23°12.00'",
  dm3 = "23°12.00`",
  dm4 = "23°12,00`",
  dm5 = "23°12,612`",
  dm6 = "N 23°12,612`",
  dm7 = "S 23°12.612´",
  dm8 = "S -23°12.612´",

  e1 = "X",
  e2 = "22°11''",
  e3 = "N°21",
  e4 = "32,12° 11'",
  e5 = "N 23° 11.2' 23.2''"
)

test_that("decimal degree coordinates are parsed correctly", {
  expect_equal(unify_coords(cases["dd1"]), 23)
  expect_equal(unify_coords(cases["dd2"]), 23)
  expect_equal(unify_coords(cases["dd3"]), 23.1)
  expect_equal(unify_coords(cases["dd4"]), 23.1)
  expect_equal(unify_coords(cases["dd5"]), 23.1)
  expect_equal(unify_coords(cases["dd6"]), 23)
  expect_equal(unify_coords(cases["dd7"]), 23.1)
  expect_equal(unify_coords(cases["dd8"]), 23.1)
  expect_equal(unify_coords(cases["dd9"]), 23.1)
  expect_equal(unify_coords(cases["dd10"]), 23.1)
  expect_equal(unify_coords(cases["dd11"]), 23.12)
  expect_equal(unify_coords(cases["dd12"]), 23.12)
  expect_equal(unify_coords(cases["dd13"]), -23.12)
  expect_equal(unify_coords(cases["dd14"]), -23)
})

test_that("degree-minute-second coordinates are parsed correctly", {
  expect_equal(unify_coords(cases["dms1"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms2"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms3"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms4"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms5"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms6"]), 23.50833, tolerance = .0001)
  expect_equal(unify_coords(cases["dms7"]), 23.50848, tolerance = .0001)
  expect_equal(unify_coords(cases["dms8"]), 23.50848, tolerance = .0001)
  expect_equal(unify_coords(cases["dms9"]), -23.50848, tolerance = .0001)
})

test_that("degree-decimal-minute coordinates are parsed correctly", {
  expect_equal(unify_coords(cases["dm1"]), 23.2, tolerance = .0001)
  expect_equal(unify_coords(cases["dm2"]), 23.2, tolerance = .0001)
  expect_equal(unify_coords(cases["dm3"]), 23.2, tolerance = .0001)
  expect_equal(unify_coords(cases["dm4"]), 23.2, tolerance = .0001)
  expect_equal(unify_coords(cases["dm5"]), 23.2102, tolerance = .0001)
  expect_equal(unify_coords(cases["dm6"]), 23.2102, tolerance = .0001)
  expect_equal(unify_coords(cases["dm7"]), 23.2102, tolerance = .0001)
  expect_equal(unify_coords(cases["dm8"]), -23.2102, tolerance = .0001)
})

test_that("a few crazy things are not parsed", {
  expect_equal(unify_coords(cases["e1"]), NA)
  expect_equal(unify_coords(cases["e2"]), NA)
  expect_equal(unify_coords(cases["e3"]), NA)
  expect_equal(unify_coords(cases["e4"]), NA)
  expect_equal(unify_coords(cases["e5"]), NA)
})

