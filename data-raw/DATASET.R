tibble::tribble(
  ~type_nl,                      ~type_eng,
  "Schriftelijke vraag",          "written_questions",
  "debatten",                     "debates",
  "vragen_interpelaties",         "oral_questions_and_interpellations",
  "parlementaire_initiatieven",   "parliamentary_initiatives",
  "gedachtenwisselingen",         "committee_hearings",
  "verzoekschriften",             "petitions"
) -> type_conv

usethis::use_data(DATASET, overwrite = TRUE)
