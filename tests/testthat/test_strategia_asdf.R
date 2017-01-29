test_that("Test działania strategia_asdf", {
  zagroda = c( 0, 0, 0, 0, 0, 0, 0 )
  zagroda = strategia_asdf( zagroda )
  expect_equal( c( 1, 1, 1, 1, 1, 0, 0 ), zagroda )
})

test_that("Test działania strategia_asdf", {
  zagroda = c( 1, 1, 1, 1, 1, 0, 0 )
  zagroda = strategia_asdf( zagroda )
  expect_equal( c( 1, 1, 1, 1, 1, 0, 0 ), zagroda )
})

test_that("Test działania strategia_asdf", {
  zagroda = c( 1, 1, 0, 0, 0, 0, 0 )
  zagroda = strategia_asdf( zagroda )
  expect_equal( c( 1, 1, 1, 1, 1, 0, 0 ), zagroda )
})

test_that("Test działania strategia_asdf", {
  zagroda = c( 2, 3, 1, 1, 4, 1, 1 )
  zagroda = strategia_asdf( zagroda )
  expect_equal( c( 1, 1, 1, 1, 1, 0, 0 ), zagroda )
})

