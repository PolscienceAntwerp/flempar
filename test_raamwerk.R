test_that(
  {
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="debates"
    )), c(140,4))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
    )), c(596,4))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm'
    )), c(2821,4))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="written_questions"
    )), c(564,14))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="debates"
                              , use_parallel = FALSE
    )), c(1,11))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , extra_via_fact = FALSE
    )), c(22,25))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="written_questions"
    )), c(555,2))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE
    )), c(564,2))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives"
    )), c(22,6))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="council_hearings"
                              , plen_comm = 'comm'
    )), c(66,6))
  }
)
