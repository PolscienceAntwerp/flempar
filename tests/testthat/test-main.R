
# PLENARY SESSIONS
# plen - parliamentary_initiatives

  test_that("PLEN_PI_DOC", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives")
    ), c(22,5))
  })

  test_that("PLEN_PI_SPEECH_NONPAR", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="parliamentary_initiatives"
                              , use_parallel = FALSE)
    ), c(307,5))
  })

  test_that("PLEN_PI_SPEECH_PAR", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="parliamentary_initiatives")
    ), c(307,5))
  })

  test_that("PLEN_PI_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , extra_via_fact = FALSE)
    ), c(36,24))

  })

  test_that("PLEN_DB_SPEECH", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="debates")
    ), c(140,5))

  })

  test_that("PLEN_DB_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="debates")
    ), c(1,18))
  })

  test_that("PLEN_OQ_SPEECH", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations")
    ), c(1222,5))
  })

   test_that("PLEN_OQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="oral_questions_and_interpellations")
    ), c(43,17))

   })

   test_that("COMM_PI_DOCUMENT", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="document"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm')
    ), c(233,5))

   })

   test_that("COMM_PI_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2008-01-01"
                              , date_range_to="2008-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm')
    ), c(75,30))
   })


    test_that("COMM_OQ_SPEECH", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm')
    ), c(1834,5))
   })

   test_that("COMM_OQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="details"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm')
    ), c(127,17))
   })

   test_that("COMM_CH_DOCUMENT", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="document"
                              , fact="committee_hearings"
                              , plen_comm = 'comm')
    ), c(22,5))
   })

   test_that("COMM_CH_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="details"
                              , fact="committee_hearings"
                              , plen_comm = 'comm')
    ), c(22,26))
   })

   test_that("WQ_DETAILS", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-10"
                              , type="details"
                              , fact="written_questions")
    ), c(191,16))
   })

   test_that("WQ_DOCUMENT", {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-15"
                              , type="document"
                              , fact="written_questions")
    ), c(307,3))

   })

   test_that("WQ_DOCUMENT_TWOCOLL", {
      expect_equal(dim(get_work(date_range_from="2002-01-01"
                              , date_range_to="2002-01-15"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE
                              , two_columns_pdf = TRUE)
    ), c(27,3))

   })

  test_that("WQ_DOCUMENT_SCND_GEN", {
    expect_equal(dim(get_work(date_range_from="2006-01-01"
                              , date_range_to="2006-01-31"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE)
    ), c(383,3))
  })

