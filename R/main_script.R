#' Call an API once
#'
#' @param URL endpoint url
#' @param path the path
#' @param query the actual query (with list())
#' @param ... whichever to add behind path
#' @importFrom dplyr %>%
call_api_once <- function(URL,path=NULL,query,...){

  if(is.null(path)){

    response <- httr::GET( file.path(URL,...), query=query,httr::accept_json())

  } else {

    response <- httr::GET( file.path(URL,path,...), query=query,httr::accept_json())

  }

  if(any(as.numeric(httr::status_code(response))%in%200:204)){

  }else{

    stop("Error: ", httr::http_status(response)[[1]])
  }

  text <- httr::content(response,"text")

  if(!identical(httr::headers(response)$`content-type`,"application/json;charset=UTF-8")){

    stop("File returned is not in JSON format, function stopped")
  }

  robj <- jsonlite::fromJSON(text)

  return(robj)

}


#' Call an API multiple times
#'
#' @param iterator vector to iterate over
#' @param URL  endpoint url
#' @param path the path
#' @param query  the actual query (with list())
#' @param resultVector the vector to prune
#' @param use_parallel whether or not to use multiple workers
#'
#' @importFrom foreach %dopar%
#' @importFrom utils object.size
#' @importFrom dplyr %>%
call_api_multiple_times <- function(iterator, URL, path, query, resultVector,use_parallel=TRUE){

  message(crayon::green(cli::symbol$tick,"Making",length(iterator),"calls." ))

  if(use_parallel==TRUE){

    if(parallel::detectCores()==1){

      stop("You only have one core, dividing the work over cores is not possible. Please set 'use_parallel=FALSE'. ")

    }

    message(crayon::green(cli::symbol$tick,"Dividing the calls between", parallel::detectCores() - 1, "workers."))

    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))

    time_used <- system.time({

      list <- foreach::foreach(i = seq_along(iterator),
                               .packages=c("dplyr","purrr","httr","jsonlite"),
                               .export=c("call_api_once"),
                               .errorhandling="stop") %dopar% {

                                 if(length(URL)==1){

                                   call_api_once(URL[[1]],
                                                 path,
                                                 query,
                                                 iterator[[i]])  -> df

                                 } else {

                                   call_api_once(URL[[i]],
                                                 path,
                                                 query,
                                                 iterator[[i]])  -> df

                                 }


                                 if(!is.null(resultVector)){

                                   purrr::pluck(df,!!!resultVector) %>% return

                                 } else{

                                   df %>% return

                                 }
                               }#endparallel

    })#endtiming

    message(crayon::green(cli::symbol$tick,"Made",length(iterator),"calls in ", round(time_used[[3]],1), "seconds."))

    names(list) <- iterator

  }#end if

  if(use_parallel==FALSE){

    list <- vector(mode="list",length= length(iterator))

    message(crayon::green(cli::symbol$tick,"Getting the data. Take into account that dividing the tasks between workers by setting 'use_parallel=TRUE' may be much faster."))

    time_used <- system.time({

      for(i in seq_along(iterator)){

        tryCatch({

          if(length(URL)==1){

            call_api_once(URL,
                          path,
                          query,
                          iterator[[i]])  -> df

          } else {

            call_api_once(URL[[i]],
                          path,
                          query,
                          iterator[[i]])  -> df

          }

          if(!is.null(resultVector)){

            purrr::pluck(df,!!!resultVector) %>% return

          } else{

            df %>% return

          }

        },error=function(e){
          return(toString(e))
          stop()
        }
        ) ->  list[[i]]

      }

    })#endtiming

    message(crayon::green(cli::symbol$tick,"Made ",length(iterator)," calls in ", round(time_used[[3]],1), "seconds."))

    names(list) <- iterator

  }#end if

  return(list)

}#endfunction


#' Guarantee that certain variables are present
#'
#' @param df dataframe
#' @param fields the field you want to guarantee
#' @importFrom dplyr %>%
guarantee_field <- function(df, fields) {
  add <-fields[!fields%in%names(df)]

  if(length(add)!=0) df[add] <- NA
  df
}


#' Get current and past legislatures of the Flemish parliament
#'
#' @importFrom dplyr %>%
#' @export
#' @examples
#'
#' \dontrun{
#'
#' get_legislatures()
#' }
get_legislatures <- function(){

  robj <- call_api_once(URL="https://ws.vlpar.be/e/opendata/leg/alle"
                        ,path=paste0("")
                        ,query=list())

  tibble::tibble(robj$items) %>%
    tidyr::unnest(cols = c(legislatuur),keep_empty = TRUE) %>%
    dplyr::select(id,start=`start-legislatuur`,eind=`eind-legislatuur`,naam,verkiezingsdatum  ) %>%
    dplyr::mutate(start = lubridate::ymd_hms(start)) %>%
    dplyr::mutate(eind = lubridate::ymd_hms(eind )) %>%
    dplyr::arrange(id)-> leg

  return(leg)

}


#' Get all the types of facts present in the query API
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'
#' \dontrun{
#' get_legislatures()
#' }
get_all_agg_types <- function(){

  robj <- call_api_once(URL="https://ws.vlpar.be/api/search/query"
                        ,path=paste0("de")
                        ,query=list(page=1))

  tibble::tibble(list = robj$facet_category) %>%
    tidyr::unnest_wider(list) %>%
    tidyr::unnest(facet) %>%
    tidyr::unnest(facet) %>%
    dplyr::filter(name=="Aggregaat Type") -> type

  return(type)

}


#' Get data by using the native query engine
#'
#' @param type Vector of types, see get_all_agg_types() for all options
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @importFrom dplyr %>%
use_generalized_query <- function( type = "Schriftelijke vraag",date_range_from,date_range_to){

  if(any(is.na(c(lubridate::ymd(date_range_from),lubridate::ymd(date_range_to))))){

    stop("Wrong Date Format, please use yyyy-mm-dd.")

  }

  date_range_from_conv <- lubridate::ymd(date_range_from) %>% format('%d%m%Y')
  date_range_to_conv <- lubridate::ymd(date_range_to) %>% format('%d%m%Y')


  robj <- call_api_once(URL="https://ws.vlpar.be/e/opendata/zj/alle"
                        ,path=paste0("")
                        ,query=list())

  tibble::tibble(robj$items) %>%
    tidyr::unnest(cols = c(zittingsjaar),keep_empty = TRUE) %>%
    dplyr::mutate(eind = lubridate::ymd_hms(`eind-zittingsjaar` )) %>%
    dplyr::mutate(begin = lubridate::ymd_hms(`start-zittingsjaar` )) %>%
    dplyr::select(begin,eind,zittingsjaar=naam,id) -> time

  time %>%
    dplyr::filter(eind >= lubridate::date(date_range_from) & begin <= lubridate::date(date_range_to) ) -> zittingsjaar

  type <- gsub(" ","%20",type)

  list <- vector("list",length=length(type))
  for(h in seq_along(1:length(type))){

    list[[h]] <- vector("list",length=length(zittingsjaar$zittingsjaar))

    for(i in seq_along(1:length(zittingsjaar$zittingsjaar))){

      robj <- call_api_once(URL="https://ws.vlpar.be/api/search/query"
                            ,path=paste0("inmeta:zittingsjaar=",zittingsjaar$zittingsjaar[[i]],"&requiredfields=aggregaattype:",type[[h]])
                            ,query=list(page=1,max=100,sort="date"))

      as.numeric(robj$count)

      if(as.numeric(robj$count) >= 10000){ #error als je over het aantal items gaat! (max 10000)

        stop("Item count is above 10000: ",robj$count," the api is limited to 10 000 search results. ")

      }

      count_pages <- ceiling(as.numeric(robj$count)/100)

      list[[h]][[i]] <- vector(mode="list",length= count_pages)

      for(j in seq_along(1:count_pages)){

        robj <- call_api_once(URL="https://ws.vlpar.be/api/search/query"
                              ,path=paste0("inmeta:zittingsjaar=",zittingsjaar$zittingsjaar[[i]],"&requiredfields=aggregaattype:",type[[h]])
                              ,query=list(page=j,max=100,sort="date"))

        list[[h]][[i]][[j]] <- robj$result

        # message(c(type[[h]]," ",zittingsjaar$zittingsjaar[[i]]," Page:",j))
      }

    }

  }

  tibble::tibble(list = list) %>%
    tidyr::unnest(list,keep_empty = TRUE) %>%
    tidyr::unnest(list,keep_empty = TRUE) %>%
    tidyr::unnest(list,keep_empty = TRUE) %>%
    tidyr::unnest(metatags,keep_empty = TRUE)  %>%
    tidyr::unnest(metatag,keep_empty = TRUE)  %>%
    dplyr::distinct() %>%
    dplyr::filter(name %in% c("publicatiedatum","opendata","document","aggregaattype")) %>%
    tidyr::pivot_wider(names_from = name,values_from = value) %>%
    dplyr::mutate(publicatiedatum = lubridate::date(publicatiedatum)) %>%
    dplyr::filter(publicatiedatum >= date_range_from & publicatiedatum <=date_range_to ) %>%
    dplyr::mutate(id_fact = stringr::str_sub(opendata,start=-7)) %>%
    dplyr::mutate(url = stringr::str_sub(opendata,end=-8)) %>%
    dplyr::select(-id,-index,-rank,-snippet)%>%
    dplyr::select(id_fact, dplyr::everything()) -> mainlist

  return(mainlist)

}


#' Parse PDF documents
#'
#' @param mainlist Data frame with documents to be parsed
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @importFrom foreach %dopar%
#' @importFrom utils object.size
#' @importFrom dplyr %>%
parse_documents <- function(mainlist,use_parallel=FALSE){

  message(crayon::green(cli::symbol$tick,"Making",length(mainlist$document),"calls." ))

  if(use_parallel==TRUE){

    if(parallel::detectCores()==1){

      stop("You only have one core, dividing the work over cores is not possible. Please set 'use_parallel=FALSE'. ")

    }

    message(crayon::green(cli::symbol$tick,"Dividing the calls between", parallel::detectCores() - 1, "workers."))

    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))

    time_used <- system.time({

      list <- foreach::foreach(i = seq_along(1:length(mainlist$document)),
                               .packages=c("dplyr","purrr","httr","jsonlite","pdftools","stringr"),
                               .errorhandling="remove") %dopar% {

                                 pdftools::pdf_text(mainlist$document[[i]]) %>%
                                   paste(sep = " ") %>%
                                   stringr::str_replace_all( stringr::fixed("\n"), " ") %>%
                                   stringr::str_replace_all( stringr::fixed("\r"), " ") %>%
                                   stringr::str_replace_all( stringr::fixed("\t"), " ") %>%
                                   stringr::str_replace_all( stringr::fixed("\""), " ") %>%
                                   paste(sep = " ", collapse = " ") %>%
                                   stringr::str_squish() %>%
                                   stringr::str_replace_all("- ", "") -> text

                                 cbind(
                                   mainlist[i,]
                                   ,data.frame(text = text)
                                 )

                               }#endparallel

    })#endtiming


  }#end parallel true

  if(use_parallel==FALSE){

    time_used <- system.time({

      list <- vector(mode="list",length= length(mainlist$document))

      for(i in seq_along(1:length(mainlist$document))){

        skip_to_next <- FALSE

        tryCatch({

          pdftools::pdf_text(mainlist$document[[i]]) %>%
            paste(sep = " ") %>%
            stringr::str_replace_all( stringr::fixed("\n"), " ") %>%
            stringr::str_replace_all( stringr::fixed("\r"), " ") %>%
            stringr::str_replace_all( stringr::fixed("\t"), " ") %>%
            stringr::str_replace_all( stringr::fixed("\""), " ") %>%
            paste(sep = " ", collapse = " ") %>%
            stringr::str_squish() %>%
            stringr::str_replace_all("- ", "") -> text

          cbind(
            mainlist[i,]
            ,data.frame(text = text)
          ) -> list[[i]]},

          error=function(e){

            cbind(
              mainlist[i,]
              ,data.frame(text = "empty")
            ) -> list[[i]]

            skip_to_next <<- TRUE
          }
        )
        if(skip_to_next) { next }

        #message(i," ",mainlist$aggregaattype[[i]])

      }

    })#endtiming


  }# end parallel false

  message(crayon::green(cli::symbol$tick,"Made",length(mainlist$document),"calls in", round(time_used[[3]],1), "seconds."))

  return(list)

}


#' Get the PDF documents of the written questions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @importFrom dplyr %>%
get_written_questions_documents <- function(date_range_from,date_range_to,use_parallel=FALSE){

  message(crayon::green(cli::symbol$tick,"Getting details on the documents." ))

  mainlist <- use_generalized_query(date_range_from=date_range_from,date_range_to=date_range_to)

  message(crayon::green(cli::symbol$tick,"Getting and parsing the documents." ))

  list <- parse_documents(mainlist=mainlist,use_parallel=use_parallel)

  tibble::tibble(list=list ) %>%
    tidyr::unnest(list,keep_empty = TRUE) -> result

  return(result)

} #end function


#' Get the basic data of the written questions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @importFrom dplyr %>%
get_written_questions_basicdata <- function(date_range_from,date_range_to,use_parallel=FALSE){

  message(crayon::green(cli::symbol$tick,"Searching for written questions." ))

  list <- use_generalized_query(date_range_from=date_range_from,date_range_to=date_range_to)

  message(crayon::green(cli::symbol$tick,"Getting the details on the written questions." ))

  result <- call_api_multiple_times(iterator=list$id_fact,
                                    URL = list$url,
                                    path = NULL,
                                    query =  list(),
                                    resultVector = NULL,
                                    use_parallel=use_parallel)

  result %>%
    tibble::tibble(result = ., id_fact = names(result)) %>%
    tidyr::unnest_wider(result,names_sep="_") %>%
    tidyr::unnest_wider(result_thema,names_sep="_") %>%
    tidyr::unnest_wider(result_vraagsteller,names_sep="_") %>%
    tidyr::unnest_wider(result_minister,names_sep="_") %>%
    # tidyr::unnest_wider(result_procedureverloop,names_sep="_") %>%
    # tidyr::unnest(c("result_procedureverloop_datum","result_procedureverloop_document","result_procedureverloop_status"))
    dplyr::select(id_fact
                  ,title=result_titel
                  ,onderwerp=result_onderwerp
                  ,dplyr::starts_with("result_thema")
                  ,zittingsjaar=result_zittingsjaar
                  ,naam_vragensteller = result_vraagsteller_naam
                  ,voornaam_vragensteller =result_vraagsteller_voornaam
                  ,id_vragensteller=result_vraagsteller_id
                  ,bevraagde_minister_naam=result_minister_naam
                  ,bevraagde_minister_voornaam=result_minister_voornaam
                  ,bevraagde_minister_id=result_minister_id
                  ,result_procedureverloop) -> result_details

  return(result_details)

}

#' Get the details of sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#'
#' get_sessions_details(date_range_from="2021-09-01"
#'                      , date_range_to="2021-12-31"
#'                      , plen_comm = "plen"
#'                      , use_parallel=TRUE)
#'
#' }
get_sessions_details <- function(date_range_from,date_range_to,plen_comm = "plen",use_parallel=FALSE,raw=FALSE){

  # warnings ----------------------------------------------------------------


  if( packageVersion("tidyr")<"1.2.0") {

    stop(paste0("You are currently using Tidyr version ",packageVersion("tidyr"),". The minimal requirement to use this function is version '1.2.0'. Please update tidyr by restarting R and running install.packages('tidyr'). "))

  }

  if(any(is.na(c(lubridate::ymd(date_range_from),lubridate::ymd(date_range_to))))){

    stop("Wrong Date Format, please use yyyy-mm-dd.")

  }

  # get data ----------------------------------------------------------------

  date_range_from_conv <- lubridate::ymd(date_range_from) %>% format('%d%m%Y')
  date_range_to_conv <- lubridate::ymd(date_range_to) %>% format('%d%m%Y')

  robj <- call_api_once(URL="http://ws.vlpar.be/e/opendata/",
                        path="/verg/vorige",
                        query=list(type=plen_comm, dagen=999999,limiet=999999,datumvan=date_range_from_conv,datumtot=date_range_to_conv))

  iterator <- purrr::pluck(robj,"items","vergadering","id")

  if(is.null(iterator)){

    stop("No sessions found between ",date_range_from," and ",date_range_to,".")
  }

  message(crayon::green(cli::symbol$tick,"Found", length(iterator), "sessions between",date_range_from, "and",date_range_to,"." ))

  message(crayon::green(cli::symbol$tick,"Getting the session details." ))

  mainlist <- call_api_multiple_times(iterator=iterator,
                                      URL = "http://ws.vlpar.be/e/opendata/",
                                      path = "verg",
                                      query = list(aanpassingen="nee",idPrsHighlight=0),
                                      resultVector = c("vergadering"),
                                      use_parallel=use_parallel)

  if(raw==TRUE){

    return(mainlist)

  }

  # transform data ----------------------------------------------------------
  if(plen_comm=="plen"){

    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg,names_sep="_") %>%
      tidyr::unnest_wider(verg_journaallijn,names_sep="_") %>%
      tidyr::unnest_wider(verg_plenairehandelingen,names_sep="_") %>%
      dplyr::select(id_verg
                    ,datumbegin = verg_datumbegin
                    ,datumeinde = verg_datumeinde
                    ,pdf_sessie=verg_plenairehandelingen_pdffilewebpath
                    ,journaallijn_id=verg_journaallijn_id
                    ,debatten=verg_journaallijn_debat
                    ,gedachtenwisselingen=verg_journaallijn_gedachtewisseling
                    ,vragen_interpelaties=verg_journaallijn_vrageninterpellatie
                    ,parlementaire_initiatieven=`verg_journaallijn_parlementair-initiatief`
                    ,verzoekschrift=verg_journaallijn_verzoekschrift) %>%
      tidyr::unnest(cols = c(journaallijn_id, debatten, gedachtenwisselingen, vragen_interpelaties,
                             parlementaire_initiatieven, verzoekschrift),keep_empty = TRUE) %>%
      tidyr::pivot_longer(cols = c(debatten
                                   ,gedachtenwisselingen
                                   ,vragen_interpelaties
                                   ,parlementaire_initiatieven
                                   ,verzoekschrift) , names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value,names_repair = "unique") %>%
      dplyr::filter(!is.na(id)) %>%
      tidyr::unnest(cols = c(id,contacttype, link, objecttype, onderwerp, titel,filewebpath, zittingsjaar),names_sep="_",keep_empty = TRUE) %>%
      tidyr::unnest_wider(link,names_sep="_") %>%
      dplyr::select(id_verg
                    ,datumbegin
                    ,datumeinde
                    ,pdf_sessie
                    ,journaallijn_id
                    ,id_fact=id
                    ,fact_link=link_href
                    ,type_activiteit
                    ,type_specifiek= objecttype_naam
                    ,onderwerp
                    ,titel
                    ,link_pdf=filewebpath
                    ,contacttype ) %>%
      dplyr::distinct() -> result

    return(result)

  }

  if(plen_comm=="comm"){

    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg,names_sep="_") %>%
      tidyr::unnest(`verg_agenda-item`,names_sep="_",keep_empty = TRUE) %>%
      tidyr::unnest_wider(`verg_agenda-item_agenda-lijn`) %>%
      dplyr::select(id_verg
                    ,debatten=debat
                    ,gedachtenwisselingen=gedachtewisseling
                    ,vragen_interpelaties=vrageninterpellatie
                    ,parlementaire_initiatieven=`parlementair-initiatief`
                    ,verzoekschrift = verzoekschrift
                    ,datumbegin = verg_datumbegin
                    ,datumeinde = verg_datumeinde
                    ,commissie = verg_commissie
                     ) %>%
      tidyr::unnest(cols = c(debatten, gedachtenwisselingen, vragen_interpelaties,
                             parlementaire_initiatieven, verzoekschrift),keep_empty = TRUE) %>%
      tidyr::pivot_longer(cols = c(debatten
                                   ,gedachtenwisselingen
                                   ,vragen_interpelaties
                                   ,parlementaire_initiatieven
                                   ,verzoekschrift) , names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value,names_repair = "unique") %>%
      dplyr::filter(!is.na(id)) %>%
      guarantee_field("filewebpath") %>%
      tidyr::unnest(cols = c(id,contacttype, link, objecttype, onderwerp, titel,filewebpath, zittingsjaar),names_sep="_",keep_empty = TRUE) %>%
      tidyr::unnest_wider(link,names_sep="_") %>%
      dplyr::select(id_verg
                    ,datumbegin
                    ,datumeinde
                    ,id_fact=id
                    ,fact_link=link_href
                    ,type_activiteit
                    ,type_specifiek= objecttype_naam
                    ,onderwerp
                    ,titel
                    ,link_pdf=filewebpath
                    ,contacttype) %>%
      dplyr::distinct() -> result

    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg,names_sep="_") %>%
      tidyr::unnest_wider(verg_journaallijn,names_sep="_") %>%
      dplyr::select(id_verg
                    ,journaallijn_id=verg_journaallijn_id
                    ,debatten=verg_journaallijn_debat
                    ,gedachtenwisselingen=verg_journaallijn_gedachtewisseling
                    ,vragen_interpelaties=verg_journaallijn_vrageninterpellatie
                    ,parlementaire_initiatieven=`verg_journaallijn_parlementair-initiatief`
                    ,verzoekschrift=verg_journaallijn_verzoekschrift) %>%
      tidyr::unnest(cols = c(journaallijn_id, debatten, gedachtenwisselingen, vragen_interpelaties,
                             parlementaire_initiatieven, verzoekschrift),keep_empty = TRUE) %>%
      tidyr::pivot_longer(cols = c(debatten
                                   ,gedachtenwisselingen
                                   ,vragen_interpelaties
                                   ,parlementaire_initiatieven
                                   ,verzoekschrift) , names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value,names_repair = "unique") %>%
      dplyr::filter(!is.na(id)) %>%
      tidyr::unnest(cols = c(id, link),names_sep="_",keep_empty = TRUE) %>%
      tidyr::unnest_wider(link,names_sep="_") %>%
      dplyr::select(id_verg
                    ,journaallijn_id
                    ,id_fact=id) %>%
      dplyr::distinct() -> jln

    # Dit klopt (check met verg_id 1594854), maar onduidelijk waarom er soms missen
    # mainlist %>%
    #   tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
    #   tidyr::unnest_wider(verg) %>%
    #   select(id_verg,commissieverslag) %>%
    #   tidyr::unnest_longer(commissieverslag) %>%
    #   tidyr::unnest(cols=c(commissieverslag )) %>%
    #   tidyr::unnest(cols = c(initiatief, verslag, hoorzitting_gedachtenwisseling, verzoekschrift),names_sep = "_") %>%
    #   dplyr::select(id_verg,initiatief_id,verslag_filewebpath) %>%
    #   dplyr::filter(!is.na(initiatief_id)|is.na(verslag_filewebpath))  -> extra_doc

    result %>%
      dplyr::left_join(jln, by=c("id_verg"="id_verg","id_fact"="id_fact")) -> result_joined

  }

  return(result_joined)
}


#' Get the speech of either plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search.
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: Should the raw object be returned?
#' @importFrom dplyr %>%
get_plen_comm_speech <- function(date_range_from,date_range_to,fact,plen_comm = "plen",use_parallel=FALSE,raw=FALSE) {

  tibble::tribble(
    ~type_nl,                      ~type_eng,
    "Schriftelijke vraag",          "written_questions",
    "debatten",                     "debates",
    "vragen_interpelaties",         "oral_questions_and_interpellations",
    "parlementaire_initiatieven",   "parlementary_initiatives",
    "gedachtenwisselingen",         "council_hearings",
    "verzoekschriften",             "petitions"
  ) -> type_conv

  session_object <- get_sessions_details(date_range_from=date_range_from
                                         ,date_range_to=date_range_to
                                         ,use_parallel=use_parallel
                                         ,plen_comm=plen_comm)

  session_object %>%
    dplyr::left_join(type_conv,by=c("type_activiteit"="type_nl"))%>%
    dplyr::filter(type_eng%in%fact) %>%
    dplyr::filter(!is.na(journaallijn_id)) -> session_object

  if(length(session_object$id_verg)==0){

    stop("No facts found. Usually this means the type of fact you are looking for did not occur during this time. Or that this fact does not contain spoken data.")

  }

  message(crayon::green(cli::symbol$tick,"Getting the raw text." ))


  result <- call_api_multiple_times(iterator=unique(session_object$journaallijn_id),
                                    URL = "http://ws.vlpar.be/e/opendata/",
                                    path = "jln",
                                    query =  list(),
                                    resultVector = c("spreker"),
                                    use_parallel=use_parallel)

  if(raw==TRUE){

    return(result)

  }

  result %>%
    tibble::tibble(spreker = ., journaallijn_id = names(result)) %>%
    tidyr::unnest_wider(spreker) %>%
    tidyr::unnest_wider(persoon,names_sep="_") %>%
    dplyr::select(journaallijn_id,text=sprekertekst,sprekertitel,persoon_id ) %>%
    tidyr::unnest(cols = c(text, sprekertitel,persoon_id),keep_empty = TRUE) %>%
    as.data.frame -> raw_text_spoken

}

#' Get the basic data from plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: Should the raw object be returned?
#' @importFrom dplyr %>%
get_plen_comm_basicdata <- function(date_range_from,date_range_to,fact,plen_comm = "plen",use_parallel=FALSE,raw=FALSE) {

  # date_range_from <- "2021-01-01"
  # date_range_to  <- "2021-02-28"

  tibble::tribble(
    ~type_nl,                      ~type_eng,
    "Schriftelijke vraag",          "written_questions",
    "debatten",                     "debates",
    "vragen_interpelaties",         "oral_questions_and_interpellations",
    "parlementaire_initiatieven",   "parlementary_initiatives",
    "gedachtenwisselingen",         "council_hearings",
    "verzoekschriften",             "petitions"
  ) -> type_conv

  session_object <- get_sessions_details(date_range_from=date_range_from
                                         ,date_range_to=date_range_to
                                         ,use_parallel=use_parallel
                                         ,plen_comm=plen_comm)

  session_object %>%
    dplyr::left_join(type_conv,by=c("type_activiteit"="type_nl"))%>%
    dplyr::filter(type_eng%in%fact) %>%
    dplyr::select(fact_link) %>%
    dplyr::mutate(id = stringr::str_sub(fact_link,start=-7)) %>%
    dplyr::mutate(url = stringr::str_sub(fact_link,end=-8)) %>%
    dplyr::distinct()  -> mainlist

  if(length(mainlist$fact_link)==0){

    stop("No facts found. Usually this means the type of fact you are looking for did not occur during the specified time.")

  }

  message(crayon::green(cli::symbol$tick,"Getting and parsing the basic data." ))

  result <- call_api_multiple_times(iterator=mainlist$id,
                                    URL = mainlist$url,
                                    path = NULL,
                                    query =  list(),
                                    resultVector = NULL,
                                    use_parallel=use_parallel)

  if(raw==TRUE){

    return(result)

  }


  if(fact=="oral_questions_and_interpellations"){

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest_wider(result_thema,names_sep="_") %>%
      dplyr::select(id_fact
                    ,result_titel
                    ,result_onderwerp
                    ,dplyr::starts_with("result_thema")
                    ,result_zittingsjaar
                    ,result_objecttype
                    ,result_contacttype
                    ,result_samenhang
                    ,result_procedureverloop
                    ,result_handelingcommissie
                    ,result_handelingenplenaire
                    ,result_hasintervenienteningroupedcontactmap) %>%
      tidyr::hoist(result_contacttype,
                   naam = list("contact", 1, "naam"),
                   voornaam = list("contact", 1, "voornaam"),
                   role= list( "beschrijving"),
                   id_mp = list("contact", 1, "id")) -> result

    return(result)

  }

  if(fact=="parlementary_initiatives"){

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest_wider(result_thema,names_sep="_") %>%
      guarantee_field(c("result_materie","result_vergadering","result_document","result_commissiegroepering",'result_namens-commissie',"result_journaallijn-stemmingen","result_samenhang",
                        "result_is-basis-van","result_is-verslag-van","result_staatsblad")) %>%
      dplyr::select(id_fact
                    ,result_titel
                    ,result_onderwerp
                    ,dplyr::starts_with("result_thema")
                    ,result_materie #verandert dit in andere?
                    ,result_zittingsjaar
                    ,result_status
                    ,result_objecttype
                    ,result_contacttype
                    ,result_vergadering
                    ,result_document
                    ,result_commissiegroepering
                    ,`result_namens-commissie`
                    ,`result_journaallijn-stemmingen`
                    ,`result_parlementair-initiatief`
                    ,result_samenhang
                    ,"result_is-basis-van"
                    ,"result_is-verslag-van"
                    ,result_staatsblad) %>%
      tidyr::hoist(result_contacttype,
                   naam = list("contact", 1, "naam"),
                   voornaam = list("contact", 1, "voornaam"),
                   role= list( "beschrijving"),
                   id_mp = list("contact", 1, "id")) -> result


    return(result)

  }

  if(fact=="debates"){

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest_wider(result_thema,names_sep="_") %>%

      dplyr::select(id_fact
                    ,result_titel
                    ,result_onderwerp
                    ,dplyr::starts_with("result_thema")
                    ,result_zittingsjaar
                    ,result_status
                    ,result_objecttype
                    ,result_contacttype
                    ,result_samenhang
                    ,result_spreker
                    ,result_procedureverloop)  -> result

    return(result)

  }

  if(fact=="council_hearings"){

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest_wider(result_thema,names_sep="_") %>%

      dplyr::select(id_fact
                    ,result_titel
                    ,result_onderwerp
                    ,dplyr::starts_with("result_thema")
                    ,result_zittingsjaar
                    ,result_status
                    ,result_objecttype
                    ,result_contacttype
                    ,`result_parlementair-initiatief`
                    ,result_samenhang
                    ,result_procedureverloop   ) %>%
      tidyr::hoist(result_contacttype,
                   naam = list("contact", 1, "naam"),
                   voornaam = list("contact", 1, "voornaam"),
                   role= list( "beschrijving"),
                   id_mp = list("contact", 1, "id")) -> result

    return(result)

  }

  if(fact=="verzoekschriften"){

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest_wider(result_thema,names_sep="_") %>%

      dplyr::select(id_fact
                    ,result_titel
                    ,result_onderwerp
                    ,dplyr::starts_with("result_thema")
                    ,result_zittingsjaar
                    ,result_status
                    ,result_objecttype
                    ,result_contacttype
                    ,`result_parlementair-initiatief`
                    ,result_samenhang
                    ,result_procedureverloop   ) %>%
      tidyr::hoist(result_contacttype,
                   naam = list("contact", 1, "naam"),
                   voornaam = list("contact", 1, "voornaam"),
                   role= list( "beschrijving"),
                   id_mp = list("contact", 1, "id")) -> result

    return(result)

  }

}

#' Get the documents discussed in plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search.
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @importFrom dplyr %>%
get_plenn_comm_documents <- function(date_range_from,date_range_to,fact,plen_comm= "plen",use_parallel=FALSE,raw=FALSE){

  # Getting the documents in the details of VERG ----------------------------

  tibble::tribble(
    ~type_nl,                      ~type_eng,
    "Schriftelijke vraag",          "written_questions",
    "debatten",                     "debates",
    "vragen_interpelaties",         "oral_questions_and_interpellations",
    "parlementaire_initiatieven",   "parlementary_initiatives",
    "gedachtenwisselingen",         "council_hearings",
    "verzoekschriften",             "petitions"
  ) -> type_conv

  session_object <- get_sessions_details(date_range_from=date_range_from
                                         ,date_range_to=date_range_to
                                         ,use_parallel=use_parallel
                                         ,plen_comm=plen_comm)

  session_object %>%
    dplyr::left_join(type_conv,by=c("type_activiteit"="type_nl")) %>%
    dplyr::filter(type_eng%in%fact) %>%
    dplyr::filter(!is.na(link_pdf)) %>%
    dplyr::mutate(id = stringr::str_sub(link_pdf,start=-7)) %>%
    dplyr::select(id_verg,id_fact,id,document= link_pdf ) %>%
    dplyr::distinct() -> mainlist

  # Getting the documents in the details of FACT ----------------------------

  if(fact %in% c("parlementary_initiatives","council_hearings")){

    message(crayon::green(cli::symbol$tick,"Getting extra document data." ))

    session_object %>%
      dplyr::left_join(type_conv,by=c("type_activiteit"="type_nl"))%>%
      dplyr::filter(type_eng%in%fact)%>%
      dplyr::select(id_verg,fact_link) %>%
      dplyr::mutate(id = stringr::str_sub(fact_link,start=-7)) %>%
      dplyr::mutate(url = stringr::str_sub(fact_link,end=-8)) %>%
      dplyr::distinct()  -> list

    result <- call_api_multiple_times(iterator=list$id,
                                      URL = list$url,
                                      path = NULL,
                                      query =  list(),
                                      resultVector = NULL,
                                      use_parallel=use_parallel)

    result %>%
      tibble::tibble(result = ., id_fact = names(result)) %>%
      tidyr::unnest_wider(result,names_sep="_") %>%
      tidyr::unnest(`result_parlementair-initiatief`,names_sep="_",keep_empty = TRUE) %>%
      dplyr::select(id_fact,`result_parlementair-initiatief_document`) %>%
      tidyr::unnest(`result_parlementair-initiatief_document`,names_sep="_",keep_empty = TRUE) %>%
      dplyr::rename(document=`result_parlementair-initiatief_document_url`) %>%
      dplyr::filter(!is.na(document)) %>%
      dplyr::mutate(id = stringr::str_sub(document,start=-7)) %>%
      dplyr::left_join(list %>% dplyr::select(id_verg,id),by=c("id_fact"="id")) %>%
      dplyr::select(id_verg,id_fact,id,document ) %>%
      dplyr::distinct() -> docs

    rbind(mainlist,docs) %>%
      dplyr::distinct() -> mainlist
  }

  if(length(mainlist$document)==0){

    stop("No facts found. Usually this means the type of fact you are looking for did not occur during the specified time or that the specified output is not present.")

  }

  # this needs to be done to get the verslag of other facts in council hearings!
  # result %>%
  #   tibble::tibble(result = ., id = names(result)) %>%
  #   tidyr::unnest_wider(result,names_sep="_") %>%
  #   dplyr::select(result_journaallijn) %>%
  #   tidyr::unnest_wider(result_journaallijn,names_sep="_")  %>%
  #   tidyr::unnest_wider( `result_journaallijn_vergadering`,names_sep="_") %>%
  #   tidyr::unnest_wider(result_journaallijn_vergadering_commissiehandelingen,names_sep="_") %>%
  #   dplyr::select(result_journaallijn_vergadering_commissiehandelingen_pdffilewebpath) %>%
  #   filter(!is.na(result_journaallijn_vergadering_commissiehandelingen_pdffilewebpath)) %>%
  #   distinct


  # get list of documents ---------------------------------------------------

  message(crayon::green(cli::symbol$tick,"Getting and parsing the documents." ))

  #need ID and document
  list <- parse_documents(mainlist=mainlist,use_parallel=use_parallel)

  tibble::tibble(list=list ) %>%
    tidyr::unnest(list,keep_empty = TRUE) -> result

  return(result)

}

#' Get data from the Flemish parliament
#'
#' @param type Type of data to be returned, options include "document", "speech" or "basicdata".
#' @param fact Which fact should be returned, options include "written_questions", "debates", "oral_questions_and_interpellations", "parlementary_initiatives" or "council_hearings"
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: Should the raw object be returned?
#' @export
#' @importFrom dplyr %>%
#' @examples
#'
#' \dontrun{
#'
#'  get_data(date_range_from="2022-01-20",
#'              date_range_to="2022-01-31",
#'              type="document",
#'              fact="written_questions",
#'              use_parallel=TRUE )
#'
#' }
get_data <- function(type="speech", fact="debates", date_range_from ,date_range_to,plen_comm="plen",use_parallel=FALSE,raw=FALSE){

  # Check input -------------------------------------------------------------

  type_list <- c("document","speech","basicdata")

  facts_list <- c("written_questions","debates","oral_questions_and_interpellations","parlementary_initiatives","council_hearings")

  #types
  if(!any(type_list %in%type)){

    stop(paste0("Supply valid type. Valid options are ",toString(tolower(type_list)),". Select one type."))

  }

  if(length(type)!=1){

    stop("You have selected multiple type options. Please set type to ", toString(tolower(facts)),".")

  }

  #facts
  if(!any(facts_list %in%fact)){

    stop(paste0("Supply valid type. Valid options are ",toString(tolower(facts_list)),". Select one type."))

  }

  if(length(fact)!=1){

    stop("You have selected multiple type options. Please set type to ", toString(tolower(facts_list)),".")

  }

  if("plen"%in%plen_comm & "council_hearings"%in%fact){

    stop("You have selected an incompatible combination of type and fact (Council hearings are not held in plenary sessions).")

  }

  # Getting data ------------------------------------------------------------

  # writtenquestions -> text
  if("document"%in%type & "written_questions"%in%fact){

    object <- get_written_questions_documents(date_range_from=date_range_from
                                              ,date_range_to=date_range_to
                                              ,use_parallel=use_parallel)

    object %>%
      dplyr::select(id_fact,text) %>%
      as.data.frame -> result

    return(result)


  }

  # writtenquestions -> basic
  if("basicdata"%in%type & "written_questions"%in%fact){

    object <-  get_written_questions_basicdata(date_range_from=date_range_from
                                               ,date_range_to=date_range_to
                                               ,use_parallel=use_parallel)

    return(object)

  }

  # get speech from "debates","oral_questions_and_interpellations","parlementary_iniatives","council_hearings"
  if("speech"%in%type & any(facts_list%in%fact)){

    object <-  get_plen_comm_speech(date_range_from=date_range_from
                                    ,date_range_to=date_range_to
                                    ,plen_comm=plen_comm
                                    ,fact=fact
                                    ,use_parallel=use_parallel
                                    ,raw=raw)

    if(raw==TRUE){

      return(object)

    }

    return(object)

  }

  # get basicdata from "debates","oral_questions_and_interpellations","parlementary_iniatives","council_hearings"
  if("basicdata"%in%type & any(facts_list%in%fact)){

    object <-  get_plen_comm_basicdata(date_range_from=date_range_from
                                       ,date_range_to=date_range_to
                                       ,plen_comm=plen_comm
                                       ,fact=fact
                                       ,use_parallel=use_parallel
                                       ,raw=raw)


    return(object)
  }

  # get documents from "debates","oral_questions_and_interpellations","parlementary_iniatives","council_hearings"
  if("document"%in%type & any(facts_list%in%fact)){

    object <-  get_plenn_comm_documents(date_range_from=date_range_from
                                        ,date_range_to=date_range_to
                                        ,plen_comm=plen_comm
                                        ,fact=fact
                                        ,use_parallel=use_parallel
                                        ,raw=raw)
    return(object)
  }


}


#' Filter texts fields on certain search terms
#'
#' @param df Data frame
#' @param text_field Which textfield should be searched?
#' @param search_terms The search terms. Multiple terms are possible by adding them as a vector with c(). This is not case sensitive.
#' @export
#' @importFrom dplyr %>%
#' @examples
#'
#' \dontrun{
#'
#' wq_document <- get_data(date_range_from="2022-01-20",
#'                            date_range_to="2022-01-31",
#'                            type="document",
#'                            fact="written_questions",
#'                            use_parallel=TRUE )
#'
#' search_terms(wq_document, text_field = "text", search_terms = "PFOS")
#'
#' }
search_terms <- function(df,text_field,search_terms=NULL){

  message(crayon::green(cli::symbol$tick,"Scrubbing away all html-tags "))

  df %>%
    tibble::as_tibble() %>%
    dplyr::rename(text_col=!!text_field) %>%
    dplyr::mutate(text_col = gsub("\r", "", text_col)) %>%
    dplyr::mutate(text_col = gsub("\n", "", text_col)) -> raw_text


  for(i in seq_along(raw_text$text_col)){

    raw_text$text_col[[i]] <- xml2::xml_text(xml2::read_html(charToRaw(raw_text$text_col[[i]])))

  }

  raw_text %>%
    dplyr::filter(stringr::str_detect(tolower(text_col), gsub(", ","|",toString(tolower(search_terms))) )) -> result

  return(result)

}


#' Search all the MPs serving, or who served the Flemish parliament
#'
#' @param selection select either "current", at a certain date ("date") or "former"
#' @param date_at When selecting "date" in selection, provide date, should be in format "yyyy-mm-dd".
#' @param fact Options of facts to return are 'raw', 'bio', 'education', 'career','political_info', 'presences_commissions' or 'presences_plenary'
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  get_mp(use_parallel=TRUE,fact="bio",date_at="1998-01-01",selection="date")
#'
#' }
get_mp <- function(selection="current",fact="bio", date_at=NULL, use_parallel=FALSE){

  # warnings ----------------------------------------------------------------

  facts <- c('raw', 'bio', 'education', 'career','political_info', 'presences_commissions', 'presences_plenary')

  if(length(selection)!=1){

    stop("You have selected multiple selection options. Please set selection to either 'current', 'all' or 'date'.")

  }

  if(length(fact)!=1){

    stop("You have selected multiple type options. Please set type to ", toString(tolower(facts)),".")

  }

  if(any(!fact %in% facts ) ){

    stop("Not a valid type. Valid options are ", toString(tolower(facts)),". Select one type." )

  }

  # getting the data --------------------------------------------------------

  message(crayon::green(cli::symbol$tick,"Getting the data." ))


  # CURRENT
  if(selection=="current"){

    if(!is.null(date_at)){

      message("Selection is set to current, date_at will be ignored.")

      date_at=NULL

    }

    date_at_conv <- Sys.Date() %>% format('%d%m%Y')

    robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                          path = "/vv/op-datum",
                          query = list(datum=date_at_conv))

    mainlist <- call_api_multiple_times(iterator=robj$items$volksvertegenwoordiger$id,
                                        URL = "http://ws.vlpar.be/e/opendata/",
                                        path = "/vv",
                                        query = list(lang="nl"),
                                        resultVector=NULL,
                                        use_parallel=use_parallel)
  }

  # DATE
  if(selection=="date"){

    if(is.null(date_at)){

      stop("You have selected date as selection criteria, but failed to provide a date. Please set date_at (yyyy-mm-dd).")

    }

    if(is.na(lubridate::ymd(date_at))){

      stop("Wrong Date Format, please use yyyy-mm-dd.")

    }

    date_at_conv <- lubridate::ymd(date_at) %>% format('%d%m%Y')

    robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                          path = "/vv/op-datum",
                          query = list(datum=date_at_conv))

    mainlist <- call_api_multiple_times(iterator=robj$items$volksvertegenwoordiger$id,
                                        URL = "http://ws.vlpar.be/e/opendata/",
                                        path = "/vv",
                                        query = list(lang="nl"),
                                        resultVector=NULL,
                                        use_parallel=use_parallel)


  }

  # FORMER
  if(selection=="former"){

    if(!is.null(date_at)){

      message("Selection is set to former, date_at will be ignored")

      date_at=NULL

    }

    robj <- call_api_once(URL = "http://ws.vlpar.be/e/opendata/",
                          path = "/vv/gewezen",
                          query = list())

    mainlist <- call_api_multiple_times(iterator=robj$items$volksvertegenwoordiger$id,
                                        URL = "http://ws.vlpar.be/e/opendata/",
                                        path = "/vv",
                                        query = list(lang="nl"),
                                        resultVector=NULL,
                                        use_parallel=use_parallel)

  }

  # data transformations ----------------------------------------------------

  fact <- tolower(fact)

  if(fact=="raw"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) -> result

    return(result)

  }

  if(fact=="bio"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id_mp=id,voornaam, achternaam=naam,geslacht,geboortedatum,geboorteplaats,gsmnr,email,website) %>%
      dplyr::mutate(geboortedatum = lubridate::date(lubridate::ymd_hms(geboortedatum))) %>%
      tidyr::unnest_wider(email ,names_sep="_")  %>%
      tidyr::unnest(website ,names_sep="_",keep_empty =TRUE) %>%
      guarantee_field(c("website_soort","website_value")) %>%
      tidyr::pivot_wider(names_from = website_soort,values_from = website_value) %>%
      dplyr::select(-`NA`) -> result

    return(result)

  }

  if(fact=="education"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("opleiding")) %>%
      dplyr::select(id,voornaam,naam, opleiding) %>%
      tidyr::unnest(opleiding,keep_empty = TRUE) -> result

    return(result)

  }

  if(fact=="career"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("beroep")) %>%
      dplyr::select(id,voornaam,naam, beroep) %>%
      tidyr::unnest(beroep,keep_empty = TRUE) %>%
      guarantee_field(c("datumvanformaat","datumtotformaat")) %>%
      dplyr::select(-datumtotformaat,-datumvanformaat) %>%
      dplyr::select(id,voornaam,naam,datumvan,datumtot,titel,werkgever)-> result

    return(result)

  }

  if(fact=="presences_commissions"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id,voornaam,naam, aanwezigheden=`aanwezigheden-huidige-legislatuur`) %>%
      tidyr::unnest_longer(aanwezigheden) %>%
      dplyr::filter(aanwezigheden_id=="commissie-aanw") %>%
      tidyr::unnest_wider(aanwezigheden) %>%
      tidyr::unnest(c(commissie, `vast-lid-aanwezigheid`, `plaatsvervangend-lid-aanwezigheid`),names_sep="_",keep_empty =TRUE)  -> result

    return(result)

  }

  if(fact=="presences_plenary"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id,voornaam,naam, aanwezigheden=`aanwezigheden-huidige-legislatuur`) %>%
      tidyr::unnest_longer(aanwezigheden) %>%
      dplyr::filter(aanwezigheden_id=="plenaire-aanw") %>%
      tidyr::unnest_wider(aanwezigheden)  -> result

    return(result)

  }

  if(fact=="political_info"){

    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id
                    ,voornaam,naam
                    ,huidigefractie
                    ,mandaat_vl=`mandaat-vlaams-parlement`
                    ,mandaat_vl_andere=`mandaat-vlaams-parlement-andere`
                    ,mandaat_andere=`mandaat-andere`
                    ,kieskring
                    ,deelstaatsenator
                    ,lidmaatschap) %>%
      tidyr::unnest_wider(lidmaatschap, names_sep = "_") %>%
      tidyr::unnest_wider(huidigefractie, names_sep = "_") -> result
      #tidyr::unnest(c(`lidmaatschap_datumVan`, `lidmaatschap_fractie`, `lidmaatschap_datumTot`), names_sep = "_",keep_empty = TRUE) %>%
      #tidyr::unnest(mandaat_vl_andere,keep_empty = TRUE) %>%
      #tidyr::unnest(mandaat_andere,keep_empty = TRUE)

    return(result)

  }

}
