tibble::tribble(
  ~fact,                               ~plen_comm,   ~speech,       ~document,      ~details,
  "debates",                            "plen",      "Possible",    "Not Possible", "Possible",
  "oral_questions_and_interpellations", "plen",      "Possible",    "Not Possible", "Possible",
  "parlementary_initiatives",           "plen",      "Possible",    "Possible",     "Possible",
  "committee_hearings",                 "plen",      "Not Possible","Not Possible", "Not Possible",
  "written_questions",                   NA,        "Not Possible","Possible",     "Possible",
  "debates",                            "comm",      "Not Possible","Not Possible", "Not Possible",
  "oral_questions_and_interpellations", "comm",      "Possible",    "Not Possible", "Possible",
  "parliamentary_initiatives",          "comm",      "Possible",    "Possible",     "Possible",
  "committee_hearings",                 "comm",      "Possible",    "Possible",     "Possible"
) -> possibilities

usethis::use_data(possibilities, overwrite = TRUE)
