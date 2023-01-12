test_that(
  {
    # PLENARY SESSIONS
    # plen - parliamentary_initiatives
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives"
    )), c(22,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="parliamentary_initiatives"
                              , use_parallel = FALSE
    )), c(307,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , extra_via_fact = FALSE
    )), c(36,15))
    #plen - debates
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="debates"
    )), c(140,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="debates"
                              , use_parallel = FALSE
    )), c(1,20))
    # plen - oqai
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
    )), c(1222,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="oral_questions_and_interpellations"
    )), c(43,19))
    # COMMISSION SESSIONS
    # comm - parliamentary initiatives
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm'
    )), c(498,5))
    expect_equal(dim(get_work(date_range_from="2008-01-01"  
                              , date_range_to="2008-01-31"
                              , type="details"
                              , fact="parliamentary_initiatives"
                              , plen_comm = 'comm'
    )), c(71,24))
    # comm - oqai
    expect_equal(dim(get_work(date_range_from="2010-01-01"  
                              , date_range_to="2010-01-31"
                              , type="speech"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm'
    )), c(X,X))
    expect_equal(dim(get_work(date_range_from="2022-01-01"  
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="oral_questions_and_interpellations"
                              , plen_comm = 'comm'
    )), c(235,19))
    # comm - council hearings
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="council_hearings"
                              , plen_comm = 'comm'
    )), c(67,5))
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="council_hearings"
                              , plen_comm = 'comm'
    )), c(67,25))
    # WRITTEN QUESTIONS
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="details"
                              , fact="written_questions"
    )), c(564,14))
    
    expect_equal(dim(get_work(date_range_from="2022-01-01"
                              , date_range_to="2022-01-31"
                              , type="document"
                              , fact="written_questions"
    )), c(564,3))
    expect_equal(dim(get_work(date_range_from="2002-01-01"
                              , date_range_to="2002-01-31"
                              , type="document"
                              , fact="written_questions"
                              , extra_via_fact = FALSE
    )), c(88,3))
  }
)
