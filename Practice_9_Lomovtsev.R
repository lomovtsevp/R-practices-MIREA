#install.packages('testthat')
library('testthat')

#global parameters
epss <- c(10e-2, 10e-3, 10e-4, 10e-5, 10e-6)
n_iters <- c(10e2, 10e3, 10e4, 10e5, 10e6)
matrix_A <- diag(seq(0, 10, 1))
u <- rnorm(11)
f <- rnorm(11)

n_iters_error <- c('2', '3', 'eff', 'g', -2)

eps_error <- c(-2, 'fuf', -3)
matrix_A_error <- matrix(data = c('2', 3, 4,' fjfuhuf'), nrow = 2, ncol = 2)


# Test1 -------------------------------------------------------------------


test_that("SIM Type errors check", {
  for (e in epss){
    for (n_iter in n_iters_error)
  expect_error(SIM(matrix_A, u0 = u, f = f, n_iter = n_iter, eps=e))
  }
  for (e in eps_error){
    expect_error(SIM(matrix_A, u, f, eps=e))
  }
  expect_error(SIM(matrix_A_error, u, f))
})

test_that("SIM Dimensions check",{
  for (e in epss) {
    for (i in c(1:11))
      expect_error(SIM(A = matrix_A, u0 = u, f = f[-i], eps = e))
      expect_error(SIM(A = matrix_A, u0 = u[-i], f = f[-i], eps = e))
      expect_error(SIM(A = A, u0 = u[-i], f = f, eps = e))
      expect_error(SIM(A = A[-i, ], u0 = u[-i], f = f, eps = e))
      expect_error(SIM(A = A[-i, ], u0 = u, f = f, eps = e))
      expect_error(SIM(A = A[-i, ], u0 = u, f = f[-i], eps = e))
      expect_error(SIM(A = A[-i, ][, -i], u0 = u, f = f[-i], eps = e))
      expect_error(SIM(A = A[-i, ][, -i], u0 = u[-i], f = f, eps = e))
      expect_error(SIM(A = A[-i, ], u0 = u[-i], f = f[-i], eps = e))
      expect_error(SIM(A = A[-i, ][, -i], u0 = u[-i], f = f[-i], eps = e))
  }
})


vector_type_error <- c('22', 'tr', 'gfgfhfh',' ghy')
dts_error <- c('2', 'fgg', 1)

vector_dimension_error <- c(2,2)

test_that('Out of trend types',{
  expect_error(out_of_trend(vector_type_error, 4, 'Arifm'))
  expect_error(out_of_trend(vector_type_error, 3, 'Geom'))
  expect_error(out_of_trend(vector_type_error, 1, 'fhgugh'))
  expect_error(out_of_trend(c('fjfhuf', 2, 3, 4), 23, 'gggg'))
})

test_that('Out of trend DIMS', {
  expect_error(out_of_trend(vector_dimension_error, 2, 'Arifm'))
  expect_error(out_of_trend(vector_dimension_error, 1, 'ffifuf'))
  expect_error(out_of_trend(vector_dimension_error, 10, 'fofjigjg'))
  expect_error(out_of_trend(c(3,4,5,6,7,8), 24, 'Geom'))
  expect_error(out_of_trend(c(2,3,4,5,3), 2, 'Nik'))
  expect_error(out_of_trend(c(2), 1, 'Geom'))
})

test_that('Alter-Johns type',{
  expect_error(alter_johns(vector_type_error))
  expect_error(alter_johns(c(-3,4,5,6,7,'g')))
  expect_error(alter_johns(c('0')))
})
