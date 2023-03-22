library("dplyr")
library("flempar")
library("doParallel")
library("lubridate")

get_mp(selection="current", fact="bio", use_parallel = FALSE)
selection="current"
fact="political_info"
use_parallel = FALSE


robj <- call_api_once(URL="http://ws.vlpar.be/e/opendata/",
                      path="verg/vorige",
                      query=list(type="plen", dagen=999999,limiet=999999,datumvan="2021-09-01",datumtot="2021-12-31"))

iterator <- robj$items$vergadering$id

date_range_from="2021-09-01"
date_range_to="2021-12-31"
plen_comm="plen"
use_parallel=TRUE
type="details"
extra_via_fact=FALSE

fact="parliamentary_initiatives"


session_object <- get_sessions_details(date_range_from="2021-09-01"
                                       ,date_range_to="2021-12-31"
                                       ,use_parallel=TRUE
                                       ,plen_comm="plen")

session_object %>%
  dplyr::left_join(type_conv,by=c("type_activiteit"="type_nl")) %>%
  dplyr::filter(type_eng%in%fact) %>%
  dplyr::select(fact_link) %>%
  dplyr::mutate(id = stringr::str_extract(fact_link,"[0-9]+")) %>%
  dplyr::mutate(url = stringr::str_extract(fact_link,"[^0-9]+")) %>%
  dplyr::select(-fact_link) %>%
  dplyr::distinct()  -> mainlist


