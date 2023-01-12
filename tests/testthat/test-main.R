
# PLENARY SESSIONS
# plen - parliamentary_initiatives

  test_that("PLEN_PI_DOC", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives"
    )), c(22,5))
  })

  test_that("PLEN_PI_SPEECH", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="parliamentary_initiatives"
                              , use_parallel = FALSE
    )), c(307,5))
  })

  test_that("PLEN_PI_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="parliamentary_initiatives"
    )), c(307,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , extra_via_fact = FALSE
    )), c(36,15))

  })

  test_that("PLEN_DB_SPEECH", {
    #plen - debates
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="debates"
    )), c(140,5))

  })

  test_that("PLEN_DB_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="debates"
                              , use_parallel = FALSE
    )), c(1,20))
<<<<<<< HEAD:tests/testthat/test-main.R

  })

  test_that("PLEN_OQ_SPEECH", {
=======
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="debates"
    )), c(1,20))
>>>>>>> 75ed7cd7074100b3edb001c0572c705b7be2235d:test_raamwerk.R
    # plen - oqai
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
    )), c(1222,5))

  })

   test_that("PLEN_OQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="oral_questions_and_interpellations"
    )), c(43,19))

   })
    # COMMISSION SESSIONS
    # comm - parliamentary initiatives

   test_that("COMM_PI_DOCUMENT", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm'
    )), c(498,5))

   })

   test_that("COMM_PI_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2008-01-01"
                              , date_range_to="2008-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm'
    )), c(71,24))
<<<<<<< HEAD:tests/testthat/test-main.R

   })

   test_that("COMM_OQ_SPECEH", {
=======
    expect_equal(dim(get_work(date_range_from="2008-01-01"  
                              , date_range_to="2008-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm'
                              , use_parallel = FALSE
    )), c(71,24))
>>>>>>> 75ed7cd7074100b3edb001c0572c705b7be2235d:test_raamwerk.R
    # comm - oqai
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-30"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm'
    )), c(4264,5))

   })

   test_that("COMM_OQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm'
    )), c(235,19))

   })

   test_that("COMM_CH_DOCUMENT", {
    # comm - council hearings
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="council_hearings"
                              , plen_comm = 'comm'
    )), c(67,5))

   })

   test_that("COMM_CH_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="council_hearings"
                              , plen_comm = 'comm'
    )), c(67,25))

   })
    # WRITTEN QUESTIONS

   test_that("WQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="written_questions"
    )), c(564,14))

   })

   test_that("WQ_DOCUMENT", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="written_questions"
    )), c(564,3))

   })

   test_that("WQ_DOCUMENT_TWOCOLL", {
      expect_equal(dim(get_work(date_range_from="2002-01-01"
                              , date_range_to="2002-01-31"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE
                              , two_columns_pdf = TRUE
    )), c(88,3))

   })

  test_that("WQ_DOCUMENT_OLD", {
    expect_equal(dim(get_work(date_range_from="2002-01-01"
                              , date_range_to="2002-01-31"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE
    )), c(88,3))

  })

