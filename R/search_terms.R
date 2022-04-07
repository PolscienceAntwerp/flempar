#' Search for specific terms
#'
#' Search for specific terms in all spoken word during the plenary sessions in the Flemish parliament.
#' Requires a plenary_object created with flempar::search_plenary().
#'
#' @param plenary_object A plenary object created by search_plenary().
#' @param search_terms The search terms. Multiple terms are possible by adding them as a vector with c(). This is not case sensitive.
#' @param type Which type of activity do you want to search? Valid options are "all", "debatten","gedachtenwisselingen","vragen_interpelaties" and "parlementaire_initiatieven".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#'
#' @importFrom dplyr %>%
#'
#' @return Dataframe with the full text and the author.
#' @export
#' @examples
#' \dontrun{
#' plenary_object <- search_plenary(date_range_from = "2021-01-01",
#'                                       date_range_to= "2021-03-31",
#'                                       use_parallel=TRUE)
#' search_terms(plenary_object = plenary_object,
#'              search_terms =  c("statistiek","welzijn"),
#'              type = c("all"),
#'              use_parallel=TRUE )
#' }
search_terms <- function(plenary_object,search_terms,type="all",use_parallel=FALSE ){

  if( packageVersion("tidyr")<"1.2.0") {

    stop(paste0("You are currently using Tidyr version ",packageVersion("tidyr"),". The minimal requirement to use this function is version '1.2.0'. Please update tidyr by restarting R and running install.packages('tidyr'). "))

  }

  if( all(!c("id_plenaire_sessie","journaallijn_id","type_activiteit") %in% names(plenary_object)) ){

    stop("Not a valid procedure object!")

  }

  if(any(!type %in% c("all", "debatten","gedachtenwisselingen","vragen_interpelaties","parlementaire_initiatieven" ) ) ){

    stop("Not a valid type. Valid options are: all, debatten, gedachtenwisselingen, vragen_interpelaties, parlementaire_initiatieven. Selecting multiple types is possible." )

  }

  if(!"all" %in% type){

    plenary_object %>%
      dplyr::filter(type_activiteit %in% !!type) -> selection

  }else{

    selection <- plenary_object
  }

  mainlist <- call_api_multiple_times(iterator=unique(selection$journaallijn_id),
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "jln",
                                      query =  list(),
                                      resultVector = c("spreker"),
                                      use_parallel=use_parallel)

  message(crayon::green(cli::symbol$tick,"Size of text object = ", format(object.size(mainlist), units = "MB", digits = 1L)))

  mainlist %>%
    tibble::tibble(spreker = ., journaallijn_id = names(mainlist)) %>%
    tidyr::unnest_wider(spreker)%>%
    dplyr::select(journaallijn_id,sprekertekst,sprekertitel ) %>%
    tidyr::unnest(cols = c(sprekertekst, sprekertitel)) -> raw_text

  message(crayon::green(cli::symbol$tick,"Scrubbing away all html-tags "))

  raw_text %>%
    dplyr::mutate(sprekertekst = gsub("\r", "", sprekertekst)) %>%
    dplyr::mutate(sprekertekst = gsub("\n", "", sprekertekst)) -> raw_text

  for(i in seq_along(raw_text$sprekertekst)){

    raw_text$sprekertekst[[i]] <- xml2::xml_text(xml2::read_html(charToRaw(raw_text$sprekertekst[[i]])))

  }

  raw_text %>%
    dplyr::filter(stringr::str_detect(tolower(sprekertekst), gsub(", ","|",toString(tolower(search_terms))) )) -> result

  return(result)
}#end of function
