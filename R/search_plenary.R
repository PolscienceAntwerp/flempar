
#' Search for plenary sessions
#'
#' Search for plenary sessions of the Flemish Parliament between certain dates.
#'
#' @param date_range_from The startdate, should be in format "yyyy-mm-dd".
#' @param date_range_to The enddate, should be in format "yyyy-mm-dd".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#'
#' @importFrom dplyr %>%
#'
#' @return Returns a dataframe with IDs of the plenary sessions in the indicated date range, together with the journal IDs and activtity IDs.
#' @export
#' @examples
#'
#' \dontrun{
#' plenary_object <-  Flempar::search_plenary(date_range_from = "2021-01-01",
#'                                       date_range_to= "2021-03-31",
#'                                       use_parallel=TRUE)
#' View(plenary_object)
#' }
search_plenary <- function(date_range_from,date_range_to,use_parallel=FALSE){


  if( packageVersion("tidyr")<"1.2.0") {

    stop(paste0("You are currently using Tidyr version ",packageVersion("tidyr"),". The minimal requirement to use this function is version '1.2.0'. Please update tidyr by restarting R and running install.packages('tidyr'). "))

  }

  if(any(is.na(c(lubridate::ymd(date_range_from),lubridate::ymd(date_range_to))))){

    stop("Wrong Date Format, please use yyyy-mm-dd.")

  }

  date_range_from_conv <- lubridate::ymd(date_range_from) %>% format('%d%m%Y')
  date_range_to_conv <- lubridate::ymd(date_range_to) %>% format('%d%m%Y')

  #try toevoegen

  robj <- call_api_once(URL="http://ws.vlpar.be/e/opendata/",
                        path="/verg/vorige",
                        query=list(type="plen", dagen=999999,limiet=999999,datumvan=date_range_from_conv,datumtot=date_range_to_conv))

  iterator <-  purrr::pluck(robj,"items","vergadering","id")

  if(is.null(iterator)){

    stop("No plenary sessions found between ",date_range_from," and ",date_range_to,".")
  }

  message(crayon::green(cli::symbol$tick,"Found", length(iterator), "plenary sessions between",date_range_from, "and",date_range_to ))

  mainlist <- call_api_multiple_times(iterator=iterator,
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "verg",
                                      query = list(aanpassingen="nee",idPrsHighlight=0),
                                      resultVector = c("vergadering"),
                                      use_parallel=use_parallel)

  mainlist %>%
    tibble::tibble(verg = .) %>%
    tidyr::unnest_wider(verg) %>%
    tidyr::unnest_wider(journaallijn,names_sep="_") %>%
    dplyr::select(id_plenaire_sessie = id
           ,datumbegin
           ,datumeinde
           ,journaallijn_id
           ,debatten=journaallijn_debat
           ,gedachtenwisselingen=journaallijn_gedachtewisseling
           ,vragen_interpelaties=journaallijn_vrageninterpellatie
           ,parlementaire_initiatieven=`journaallijn_parlementair-initiatief`) %>%
    tidyr::pivot_longer(cols = c(debatten
                          ,gedachtenwisselingen
                          ,vragen_interpelaties
                          ,parlementaire_initiatieven) , names_to = "type_activiteit", values_to = "value") %>%
    tidyr::unnest(cols = c(journaallijn_id, value)) %>%
    tidyr::unnest_wider(value,names_repair = "unique") %>%
    tidyr::unnest(cols = c(contacttype, id, link, nummer, objectstatus, objecttype, onderwerp,
                    titel, verslagnognietbeschikbaar, zittingsjaar, document,
                    `namens-commissie`),names_sep="_") %>%
  dplyr::select(id_plenaire_sessie
                  ,datumbegin
                  ,datumeinde
                  ,journaallijn_id
                  ,type_activiteit
                  ,item_id = id
                  ,objecttype_naam
                  ,onderwerp =onderwerp
                  ,titel = titel_titel ) %>%
    filter(!is.na(item_id)) -> result

  result %>%
    dplyr::group_by(type_activiteit) %>%
    dplyr::tally() -> df

  message(crayon::green("Summary:"))

  for(i in seq_along(df$type_activiteit)){

    message(crayon::green("->", df$n[[i]],df$type_activiteit[[i]] ))

  }

  return(result)
}
