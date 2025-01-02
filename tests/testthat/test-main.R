# PLENARY SESSIONS
# plen - parliamentary_initiatives

test_that("PLEN_PI_DOC", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-06",
    date_range_to = "2022-01-15",
    type = "document",
    fact = "parliamentary_initiatives"
  )), c(10, 5))
})

test_that("PLEN_PI_SPEECH_NONPAR", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-16",
    date_range_to = "2022-01-22",
    type = "speech",
    fact = "parliamentary_initiatives",
    use_parallel = FALSE
  )), c(26, 5))
})

test_that("PLEN_PI_SPEECH_PAR", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-16",
    date_range_to = "2022-01-22",
    type = "speech",
    fact = "parliamentary_initiatives"
  )), c(26, 5))
})

test_that("PLEN_PI_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-16",
    date_range_to = "2022-01-22",
    type = "details",
    fact = "parliamentary_initiatives",
    extra_via_fact = FALSE
  )), c(8, 43))
})

test_that("PLEN_DB_SPEECH", {
  expect_equal(dim(get_work(
    date_range_from = "2023-05-17",
    date_range_to = "2023-05-18",
    type = "speech",
    fact = "debates"
  )), c(253, 5))
})

test_that("PLEN_DB_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2023-05-17",
    date_range_to = "2023-05-18",
    type = "details",
    fact = "debates"
  )), c(2, 25))
})

test_that("PLEN_OQ_SPEECH", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-01",
    date_range_to = "2022-01-15",
    type = "speech",
    fact = "oral_questions_and_interpellations"
  )), c(369, 5))
})

test_that("PLEN_OQ_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-01",
    date_range_to = "2022-01-15",
    type = "details",
    fact = "oral_questions_and_interpellations"
  )), c(16, 19))
})

test_that("COMM_PI_DOCUMENT", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-21",
    date_range_to = "2022-01-22",
    type = "document",
    fact = "parliamentary_initiatives",
    plen_comm = "comm"
  )), c(57, 5))
})

test_that("COMM_PI_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2008-10-19",
    date_range_to = "2008-10-22",
    type = "details",
    fact = "parliamentary_initiatives",
    plen_comm = "comm"
  )), c(16, 40))
})


test_that("COMM_OQ_SPEECH", {
  expect_equal(dim(get_work(
    date_range_from = "2022-02-09",
    date_range_to = "2022-02-10",
    type = "speech",
    fact = "oral_questions_and_interpellations",
    plen_comm = "comm"
  )), c(157, 5))
})

test_that("COMM_OQ_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2022-02-09",
    date_range_to = "2022-02-10",
    type = "details",
    fact = "oral_questions_and_interpellations",
    plen_comm = "comm"
  )), c(10, 18))
})

test_that("COMM_CH_DOCUMENT", {
  expect_equal(dim(get_work(
    date_range_from = "2022-03-29",
    date_range_to = "2022-03-30",
    type = "document",
    fact = "committee_hearings",
    plen_comm = "comm"
  )), c(12, 5))
})

test_that("COMM_CH_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2022-03-29",
    date_range_to = "2022-03-30",
    type = "details",
    fact = "committee_hearings",
    plen_comm = "comm"
  )), c(12, 25))
})

test_that("WQ_DETAILS", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-05",
    date_range_to = "2022-01-05",
    type = "details",
    fact = "written_questions"
  )), c(46, 16))
})

test_that("WQ_DOCUMENT", {
  expect_equal(dim(get_work(
    date_range_from = "2022-01-05",
    date_range_to = "2022-01-05",
    type = "document",
    fact = "written_questions"
  )), c(46, 3))
})

test_that("WQ_DOCUMENT_TWOCOLL", {
  expect_equal(dim(get_work(
    date_range_from = "2002-01-17",
    date_range_to = "2002-01-17",
    type = "document",
    fact = "written_questions",
    extra_via_fact = FALSE,
    two_columns_pdf = TRUE
  )), c(1, 3))
})

test_that("WQ_DOCUMENT_SCND_GEN", {
  expect_equal(dim(get_work(
    date_range_from = "2006-01-14",
    date_range_to = "2006-01-17",
    type = "document",
    fact = "written_questions",
    extra_via_fact = FALSE
  )), c(18, 3))
})
