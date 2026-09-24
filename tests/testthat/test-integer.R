test_that("typed integers above R's numeric range remain exact", {
  source <- '{"input_tokens":9007199254740993,"output_tokens":9007199254740995,"total_tokens":18014398509481988}'
  expect_identical(as_json(from_json(source, "usage")), source)
  value <- from_json('{"input_tokens":9007199254740993,"output_tokens":9007199254740995}', "usage")
  expect_identical(as_json(value), source)
  expect_identical(as_json(config(max_tokens = integer_value("9007199254740993"))), '{"max_tokens":9007199254740993}')
  expect_error(as.double(integer_value("9007199254740993")), "exact numeric range")
  expect_identical(as.double(integer_value("3000000000")), 3000000000)
})

test_that("exact integer operations respect signs, carries and cancellation", {
  a <- integer_value("999999999999999999999999")
  expect_identical(as_json(a + 1L), "1000000000000000000000000")
  expect_identical(as_json(a - 1L), "999999999999999999999998")
  expect_identical(a - a, 0L)
  expect_identical(as_json(-a + 1L), "-999999999999999999999998")
  expect_true(a > integer_value("999999999999999999999998"))
  expect_true(-a < -1L)
  expect_true(integer_value("3000000000") == 3000000000)
  expect_identical(as_json(integer_value("3000000000")), "3000000000")
  expect_error(integer_value("2.5"))
  expect_error(integer_value("9007199254740992.5"))
  expect_identical(as_json(integer_value("123.000e2")), "12300")
})

test_that("floating point fields accept finite large floats without treating them as integers", {
  value <- config(temperature = 1e100)
  expect_identical(as_json(from_json(as_json(value), "config")), as_json(value))
  expect_error(from_json('{"temperature":1e400}', "config"))
})
