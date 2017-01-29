test_that("Test działania funkcji badaj_gre", {
  expect_equal( c( 0 ), badaj_gre( strategia_asdf, 1 ) )
})

test_that("Test działania funkcji badaj_gre", {
  expect_equal( c( 0, 0 ), badaj_gre( strategia_asdf, 2 ) )
})

test_that("Test działania funkcji badaj_gre", {
  expect_equal( c( 0, 0, 0, 0 ), badaj_gre( strategia_asdf, 4) )
})
