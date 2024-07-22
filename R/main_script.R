#' Remove trailing and preceding / from a given string
#' @param input_string input string
remove_trailing_and_leading_slashes <- function(input_string) {
  # Remove leading slashes
  input_string <- sub("^/*", "", input_string)
  # Remove trailing slashes
  input_string <- sub("/*$", "", input_string)
  return(input_string)
}

#' Call an API once
#'
#' @param URL endpoint url
#' @param path the path
#' @param query the actual query (with list())
#' @param ... whichever to add behind path
#' @importFrom dplyr %>%
call_api_once <- function(URL, path = NULL, query, ...) {
  URL <- remove_trailing_and_leading_slashes(URL)
  if (is.null(path)) {
    response <- httr::GET(file.path(URL, ...), query = query, httr::accept_json())
  } else {
    path <- remove_trailing_and_leading_slashes(path)
    response <- httr::GET(file.path(URL, path, ...), query = query, httr::accept_json())
  }
  if (any(as.numeric(httr::status_code(response)) %in% 200:204)) {
  } else {
    stop("Error: ", httr::http_status(response)[[1]])
  }
  
  content <- httr::content(response, "text")
  
  if (!identical(httr::headers(response)$`content-type`, "application/json;charset=UTF-8")) {
    stop("File returned is not in JSON format.")
  }
  
  robj <- jsonlite::fromJSON(content)
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
#' @importFrom foreach %dopar%
#' @importFrom utils object.size
#' @importFrom dplyr %>%
call_api_multiple_times <- function(iterator, URL, path, query, resultVector, use_parallel = TRUE) {
  message("Making ", length(iterator), " calls.")
  
  if (use_parallel == TRUE) {
    if (parallel::detectCores() == 1) {
      stop("You only have one core, dividing the work over cores is not possible. Please set 'use_parallel=FALSE'. ")
    }
    
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    
    time_used <- system.time({
      list <- foreach::foreach(
        i = seq_along(iterator),
        .packages = c("dplyr", "purrr", "httr", "jsonlite"),
        .export = c("call_api_once", "remove_trailing_and_leading_slashes"),
        .errorhandling = "stop"
      ) %dopar% {
        if (length(URL) == 1) {
          call_api_once(
            URL[[1]],
            path,
            query,
            iterator[[i]]
          ) -> df
        } else {
          call_api_once(
            URL[[i]],
            path,
            query,
            iterator[[i]]
          ) -> df
        }
        
        if (!is.null(resultVector)) {
          purrr::pluck(df, !!!resultVector) %>% return()
        } else {
          df %>% return()
        }
      } # endparallel
    }) # endtiming
    
    
    message("Made ", length(iterator), " calls in ", round(time_used[[3]], 1), " seconds.")
    
    names(list) <- iterator
  } # end if
  
  if (use_parallel == FALSE) {
    list <- vector(mode = "list", length = length(iterator))
    
    message("Getting the data. Take into account that dividing the tasks between workers by setting 'use_parallel=TRUE' may be much faster.")
    
    time_used <- system.time({
      for (i in seq_along(iterator)) {
        tryCatch(
          {
            if (length(URL) == 1) {
              call_api_once(
                URL,
                path,
                query,
                iterator[[i]]
              ) -> df
            } else {
              call_api_once(
                URL[[i]],
                path,
                query,
                iterator[[i]]
              ) -> df
            }
            
            if (!is.null(resultVector)) {
              purrr::pluck(df, !!!resultVector) %>% return()
            } else {
              df %>% return()
            }
          },
          error = function(e) {
            return(toString(e))
            stop()
          }
        ) -> list[[i]]
      }
    }) # endtiming
    
    message("Made ", length(iterator), " calls in ", round(time_used[[3]], 1), " seconds.")
    
    names(list) <- iterator
  } # end if
  
  return(list)
} # endfunction


#' Guarantee that certain variables are present
#'
#' @param df dataframe
#' @param fields the field you want to guarantee
#' @importFrom dplyr %>%
guarantee_field <- function(df, fields) {
  add <- fields[!fields %in% names(df)]
  if (length(add) != 0) df[add] <- NA
  return(df)
}


#' Get current and past legislatures of the Flemish parliament
#'
#' @importFrom dplyr %>%
#' @export
#' @examples
#' \dontrun{
#' get_legislatures()
#' }
get_legislatures <- function() {
  tryCatch(
    {
      robj <- call_api_once(
        URL = "https://ws.vlpar.be/e/opendata/leg/alle",
        path = paste0(""),
        query = list()
      )
      
      tibble::tibble(robj$items) %>%
        tidyr::unnest(cols = c(legislatuur), keep_empty = TRUE) %>%
        dplyr::select(id, start = `start-legislatuur`, eind = `eind-legislatuur`, naam, verkiezingsdatum) %>%
        dplyr::mutate(start = lubridate::ymd_hms(start)) %>%
        dplyr::mutate(eind = lubridate::ymd_hms(eind)) %>%
        dplyr::arrange(id) -> legislatures
    },
    error = function(e) {
      message("An error occurred: ", e)
    }
  )
  
  if (is.null(legislatures)) {
    stop("Failed to retrieve legislatures.")
  }
  
  return(legislatures)
}


#' Get all the types of facts present in the query API (uses the ubiqutious word 'de' in search API).
#'
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' get_all_agg_types()
#' }
get_all_agg_types <- function() {
  tryCatch(
    {
      robj <- call_api_once(
        URL = "https://ws.vlpar.be/api/search/query",
        path = paste0("de"),
        query = list(page = 1)
      )
      
      tibble::tibble(list = robj$facet_category) %>%
        tidyr::unnest_wider(list) %>%
        tidyr::unnest(facet, keep_empty = TRUE) %>%
        tidyr::unnest(facet, keep_empty = TRUE) %>%
        dplyr::filter(name == "Aggregaat Type") -> types
    },
    error = function(e) {
      message("An error occurred: ", e)
    }
  )
  
  if (is.null(types)) {
    stop("Failed to retrieve types")
  }
  
  return(types)
}


#' Get data by using the native query engine (now only initiated to search written questions, but can be extended)
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param type Vector of types, see get_all_agg_types() for all options
#' @importFrom dplyr %>%
use_generalized_query <- function(date_range_from, date_range_to, type = "Schriftelijke vraag") {
  if (any(is.na(c(lubridate::ymd(date_range_from), lubridate::ymd(date_range_to))))) {
    stop("Wrong Date Format, please use yyyy-mm-dd.")
  }
  
  type <- gsub(" ", "%20", type)
  list <- vector("list", length = length(type))
  for (i in seq_along(1:length(type))) {
    robj <- call_api_once(
      URL = "https://ws.vlpar.be/api/search/query",
      path = paste0("inmeta:publicatiedatum:daterange:", date_range_from, "..", date_range_to, "&requiredfields=aggregaattype:", type[[i]]),
      query = list(page = 1, max = 100, sort = "date")
    )
    
    if (as.numeric(robj$count) >= 10000) { # error als je over het aantal items gaat! (max 10000)
      stop("Item count is above 10000: ", robj$count, " the api is limited to 10 000 search results. Please reduce the date range. ")
    }
    if (as.numeric(robj$count) == 0) { # error als je over het aantal items gaat! (max 10000)
      stop("Item count is 0, no questions found. Please try another selection. ")
    }
    
    count_pages <- ceiling(as.numeric(robj$count) / 100)
    list[[i]] <- vector(mode = "list", length = count_pages)
    for (h in seq_along(1:count_pages)) {
      robj <- call_api_once(
        URL = "https://ws.vlpar.be/api/search/query",
        path = paste0("inmeta:publicatiedatum:daterange:", date_range_from, "..", date_range_to, "&requiredfields=aggregaattype:", type[[i]]),
        query = list(page = h, max = 100, sort = "date")
      )
      list[[i]][[h]] <- robj$result
    }
  }
  
  tibble::tibble(list = list) %>%
    tidyr::unnest(list, keep_empty = TRUE) %>%
    tidyr::unnest(list, keep_empty = TRUE) %>%
    tidyr::unnest(metatags, keep_empty = TRUE) %>%
    tidyr::unnest(metatag, keep_empty = TRUE) %>%
    dplyr::distinct() %>%
    dplyr::filter(name %in% c("publicatiedatum", "opendata", "document", "aggregaattype", "mimetype", "minister", "vraagsteller")) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    dplyr::mutate(publicatiedatum = lubridate::date(publicatiedatum)) %>%
    dplyr::mutate(id_fact = stringr::str_extract(opendata, "[0-9]+")) %>%
    dplyr::mutate(url = stringr::str_extract(opendata, "[^0-9]+")) %>%
    dplyr::select(-id, -index, -rank, -snippet) %>%
    dplyr::select(id_fact, dplyr::everything()) -> mainlist
  
  if (is.null(mainlist) || nrow(mainlist) == 0) {
    stop("Failed to retrieve data.")
  }
  
  return(mainlist)
}


#' Function to read in two-column pdfs
#'
#' @param text result from pdftools call
#'
read_text <- function(text) {
  trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  QTD_COLUMNS <- 2
  result <- ""
  lstops <- gregexpr(pattern = " ", text)
  stops <- as.integer(names(sort(table(unlist(lstops)), decreasing = TRUE)[1:2]))
  
  for (i in seq(1, QTD_COLUMNS, by = 1)) {
    temp_result <- sapply(text, function(x) {
      start <- 1
      stop <- stops[i]
      if (i > 1) {
        start <- stops[i - 1] + 1
      }
      if (i == QTD_COLUMNS) {
        stop <- nchar(x) + 1
      }
      substr(x, start = start, stop = stop)
    }, USE.NAMES = FALSE)
    temp_result <- trim(temp_result)
    result <- append(result, temp_result)
  }
  return(result)
}

#' Parse PDF documents
#'
#' @param mainlist Data frame with documents to be parsed
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param two_columns_pdf Boolean: Use when you encounter two-colummned PDFs
#' @importFrom foreach %dopar%
#' @importFrom utils object.size
#' @importFrom dplyr %>%
parse_documents <- function(mainlist, use_parallel = TRUE, two_columns_pdf = FALSE) {
  message("Making ", length(mainlist$document), " calls.")
  
  if (use_parallel == TRUE) {
    if (parallel::detectCores() == 1) {
      stop("You only have one core, dividing the work over cores is not possible. Please set 'use_parallel=FALSE'. ")
    }
    
    cl <- parallel::makeCluster(parallel::detectCores() - 1)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    temp_dir <- tempdir()
    
    time_used <- system.time(
      {
        list <- foreach::foreach(
          i = seq_along(1:length(mainlist$document)),
          .packages = c("dplyr", "purrr", "httr", "jsonlite", "pdftools", "stringr", "antiword", "doconv", "officer"),
          .export = c("read_text"),
          .errorhandling = c("pass")
        ) %dopar% {
          tryCatch(
            {
              if ("mimetype" %in% names(mainlist)) {
                type <- mainlist$mimetype[i]
              } else {
                type <- httr::GET(mainlist$document[[i]])$headers$`content-type`
              }
              
              if (type == "application/msword") {
                text <- tryCatch(
                  {
                    antiword::antiword(mainlist$document[[i]]) %>%
                      stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
                      stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
                      stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
                      stringr::str_replace_all(stringr::fixed("\""), " ")
                  },
                  error = function(e) {
                    as.character(e)
                  }
                )
                if (stringr::str_detect(text, "is not a Word Document.")) {
                  doc <- curl::curl_download(mainlist$document[[i]],
                                             destfile = paste0(temp_dir, "/", i, ".doc")
                  )
                  rm(doc)
                } else {
                  cbind(
                    mainlist[i, ],
                    data.frame(text = text)
                  )
                }
              }
              else if (type == "application/pdf") {
                if (two_columns_pdf == TRUE) {
                  txt <- pdftools::pdf_text(mainlist$document[[i]])
                  result <- ""
                  for (j in 1:length(txt)) {
                    page <- txt[j]
                    t1 <- unlist(strsplit(page, "\n"))
                    maxSize <- max(nchar(t1))
                    t1 <- paste0(t1, strrep(" ", maxSize - nchar(t1)))
                    result <- append(result, read_text(t1))
                  }
                  result %>%
                    paste(sep = " ") %>%
                    stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\""), " ") %>%
                    paste(sep = " ", collapse = " ") %>%
                    stringr::str_squish() %>%
                    stringr::str_replace_all("- ", "") -> text
                }
                else {
                  pdftools::pdf_text(mainlist$document[[i]]) %>%
                    paste(sep = " ") %>%
                    stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
                    stringr::str_replace_all(stringr::fixed("\""), " ") %>%
                    paste(sep = " ", collapse = " ") %>%
                    stringr::str_squish() %>%
                    stringr::str_replace_all("- ", "") -> text
                }
                cbind(
                  mainlist[i, ],
                  data.frame(text = text)
                )
              }
              else if (type == "application/rtf") {
                cbind(
                  mainlist[i, ],
                  data.frame(text = "RTF not supported")
                )
              }
              else if (type %in% c("application/vnd.openxmlformats-officedocument.wordprocessingml.document", "binary/octet-stream", "application/octet-stream")) {
                doc <- curl::curl_download(mainlist$document[[i]],
                                           destfile = paste0(temp_dir, "/", i, ".docx")
                )
                
                officer::read_docx(doc) %>%
                  officer::docx_summary() %>%
                  dplyr::filter(content_type == "paragraph") %>%
                  dplyr::select(text) %>%
                  paste(sep = " ", collapse = " ") %>%
                  stringr::str_squish() %>%
                  stringr::str_replace_all(stringr::fixed("\""), " ") %>%
                  stringr::str_replace_all(stringr::fixed("c("), " ") -> text
                unlink(doc)
                cbind(
                  mainlist[i, ],
                  data.frame(text = text)
                )
              }
              else {
                cbind(
                  mainlist[i, ],
                  data.frame(text = "Failed: filetype not supported")
                )
              }
            },
            error = function(e) {
              cbind(
                mainlist[i, ],
                data.frame(text = "Error in parallel")
              )
            }
          )
        }
      })
  } # endparallel
  
  tibble::tibble(files = list.files(temp_dir)) %>%
    dplyr::mutate(docfiles = stringr::str_detect(files, ".doc")) %>%
    dplyr::filter(docfiles == TRUE) %>%
    dplyr::mutate(index = stringr::str_extract(files, "[0-9]+")) -> files
  
  if (!nrow(files) == 0) {
    list_recov <- vector(mode = "list", length = length(files$files))
    for (i in seq_along(1:length(files$files))) {
      text <- tryCatch(
        {
          name <- doconv::to_pdf(input = paste0(temp_dir, "/", files$files[[i]]), timeout = 12000)
          
          pdftools::pdf_text(name) %>%
            paste(sep = " ") %>%
            stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
            stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
            stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
            stringr::str_replace_all(stringr::fixed("\""), " ") %>%
            paste(sep = " ", collapse = " ") %>%
            stringr::str_squish() %>%
            stringr::str_replace_all("- ", "") -> text
        },
        error = function(e) {
          as.character(e)
        }
      )
      cbind(
        mainlist[files$index[[i]], ],
        data.frame(text = text)
      ) -> list_recov[[i]]
      if (exists("name")) {
        unlink(name)
      }
      unlink(paste0(temp_dir, "/", files$files[[i]]))
    }
    list <- append(list, list_recov)
  }
  list <- Filter(Negate(is.null), list)
  
  if (use_parallel == FALSE) {
    time_used <- system.time({
      temp_dir <- tempdir()
      
      list <- vector(mode = "list", length = length(mainlist$document))
      for (i in seq_along(1:length(mainlist$document))) {
        if ("mimetype" %in% names(mainlist)) {
          type <- mainlist$mimetype[i]
        } else {
          type <- httr::GET(mainlist$document[[i]])$headers$`content-type`
        }
        
        if (type == "application/msword") {
          text <- tryCatch(
            {
              antiword::antiword(mainlist$document[[i]]) %>%
                stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
                stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
                stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
                stringr::str_replace_all(stringr::fixed("\""), " ")
            },
            error = function(e) {
              as.character(e)
            }
          )
          
          if (stringr::str_detect(text, "is not a Word Document.")) {
            list[[i]] <- tryCatch(
              {
                doc <- curl::curl_download(mainlist$document[[i]],
                                           destfile = paste0(temp_dir, "/", i, ".doc")
                )
                name <- doconv::to_pdf(input = doc, timeout = 12000)
                unlink(name)
                unlink(doc)
                pdftools::pdf_text(name) %>%
                  paste(sep = " ") %>%
                  stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
                  stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
                  stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
                  stringr::str_replace_all(stringr::fixed("\""), " ") %>%
                  paste(sep = " ", collapse = " ") %>%
                  stringr::str_squish() %>%
                  stringr::str_replace_all("- ", "") -> text
                cbind(
                  mainlist[i, ],
                  data.frame(text = text)
                )
              },
              error = function(e) {
                cbind(
                  mainlist[i, ],
                  data.frame(text = as.character(e))
                )
              }
            )
          } else {
            cbind(
              mainlist[i, ],
              data.frame(text = text)
            ) -> list[[i]]
          }
        } else if (type == "application/pdf") {
          if (two_columns_pdf == TRUE) {
            txt <- pdftools::pdf_text(mainlist$document[[i]])
            result <- ""
            for (j in 1:length(txt)) {
              page <- txt[j]
              t1 <- unlist(strsplit(page, "\n"))
              maxSize <- max(nchar(t1))
              t1 <- paste0(t1, strrep(" ", maxSize - nchar(t1)))
              result <- append(result, read_text(t1))
            }
            result %>%
              paste(sep = " ") %>%
              stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\""), " ") %>%
              paste(sep = " ", collapse = " ") %>%
              stringr::str_squish() %>%
              stringr::str_replace_all("- ", "") -> text
          } else {
            pdftools::pdf_text(mainlist$document[[i]]) %>%
              paste(sep = " ") %>%
              stringr::str_replace_all(stringr::fixed("\n"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\r"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\t"), " ") %>%
              stringr::str_replace_all(stringr::fixed("\""), " ") %>%
              paste(sep = " ", collapse = " ") %>%
              stringr::str_squish() %>%
              stringr::str_replace_all("- ", "") -> text
          }
          cbind(
            mainlist[i, ],
            data.frame(text = text)
          ) -> list[[i]]
        } else if (type %in% c("application/rtf")) {
          cbind(
            mainlist[i, ],
            data.frame(text = "RTF not supported")
          ) -> list[[i]]
        } else if (type %in% c("application/vnd.openxmlformats-officedocument.wordprocessingml.document", "binary/octet-stream", "application/octet-stream")) {
          doc <- curl::curl_download(mainlist$document[[i]],
                                     destfile = paste0(temp_dir, "/", i, ".docx")
          )
          officer::read_docx(doc) %>%
            officer::docx_summary() %>%
            dplyr::filter(content_type == "paragraph") %>%
            dplyr::select(text) %>%
            paste(sep = " ", collapse = " ") %>%
            stringr::str_squish() %>%
            stringr::str_replace_all(stringr::fixed("\""), " ") -> text
          unlink(docx)
          cbind(
            mainlist[i, ],
            data.frame(text = text)
          ) -> list[[i]]
        } else {
          cbind(
            mainlist[i, ],
            data.frame(text = "Failed: filetype not supported")
          ) -> list[[i]]
        }
      }
    })
  }
  
  unlink(temp_dir, recursive = TRUE)
  message("Made ", length(mainlist$document), " calls in ", round(time_used[[3]], 1), " seconds.")
  
  return(list)
}


#' Get the PDF documents of the written questions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param two_columns_pdf Boolean: Use when you encounter two-colummned PDFs
#' @importFrom dplyr %>%
get_written_questions_documents <- function(date_range_from, date_range_to, use_parallel = TRUE, two_columns_pdf = FALSE) {
  message("Getting details on the documents.")
  mainlist <- use_generalized_query(date_range_from = date_range_from, date_range_to = date_range_to)
  message("Getting and parsing the documents.")
  
  list <- parse_documents(mainlist = mainlist, use_parallel = use_parallel, two_columns_pdf = two_columns_pdf)
  
  tibble::tibble(list = list) %>%
    tidyr::unnest(list, keep_empty = TRUE) %>%
    dplyr::mutate(id_fact = gsub("/", "", id_fact)) -> result
  
  return(result)
} # end function


#' Get the details of the written questions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @importFrom dplyr %>%
get_written_questions_details <- function(date_range_from, date_range_to, use_parallel = TRUE, raw = FALSE) {
  message("Searching for written questions.")
  list <- use_generalized_query(date_range_from = date_range_from, date_range_to = date_range_to)
  message("Getting the details on the written questions.")
  
  list %>%
    dplyr::mutate(id_fact = stringr::str_extract(id_fact, "[0-9]+")) %>%
    dplyr::select(-document) %>%
    dplyr::distinct() -> list
  
  result <- call_api_multiple_times(
    iterator = list$id_fact,
    URL = list$url,
    path = NULL,
    query = list(),
    resultVector = NULL,
    use_parallel = use_parallel
  )
  
  if (raw == TRUE) {
    return(result)
  }
  
  result %>%
    tibble::tibble(result = ., id_fact = names(result)) %>%
    tidyr::unnest_wider(result, names_sep = "_") %>%
    tidyr::unnest_wider(result_thema, names_sep = "_") %>%
    tidyr::unnest_wider(result_vraagsteller, names_sep = "_") %>%
    tidyr::unnest_wider(result_minister, names_sep = "_") %>%
    dplyr::select(id_fact,
                  title = result_titel,
                  onderwerp = result_onderwerp,
                  dplyr::starts_with("result_thema"),
                  zittingsjaar = result_zittingsjaar,
                  naam_vragensteller = result_vraagsteller_naam,
                  voornaam_vragensteller = result_vraagsteller_voornaam,
                  id_vragensteller = result_vraagsteller_id,
                  bevraagde_minister_naam = result_minister_naam,
                  bevraagde_minister_voornaam = result_minister_voornaam,
                  bevraagde_minister_id = result_minister_id,
                  result_procedureverloop,
                  soort_antwoord = `result_soort-antwoord`,
                  tijdig = result_tijdig
    ) -> result_details
  
  return(result_details)
}

#' Get the details of sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @param extra_via_fact Boolean: also search the underlying endpoint for linked documents? This may return documents not linked to the specific meeting, thus may also include meetings on dates before/after the date range.
#' @param type Type of data to be returned, options include "document", "speech" or "details".
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#'
#' get_sessions_details(
#'   date_range_from = "2021-09-01",
#'   date_range_to = "2021-12-31",
#'   plen_comm = "plen",
#'   use_parallel = TRUE
#' )
#' }
get_sessions_details <- function(date_range_from, date_range_to, plen_comm, type = "details", use_parallel = TRUE, raw = FALSE, extra_via_fact = FALSE) {
  if (packageVersion("tidyr") < "1.2.0") {
    stop(paste0("You are currently using Tidyr version ", packageVersion("tidyr"), ". The minimal requirement to use this function is version '1.2.0'. Please update tidyr by restarting R and running install.packages('tidyr'). "))
  }
  
  if (any(is.na(c(lubridate::ymd(date_range_from), lubridate::ymd(date_range_to))))) {
    stop("Wrong Date Format, please use yyyy-mm-dd.")
  }
  
  date_range_from_conv <- lubridate::ymd(date_range_from) %>% format("%d%m%Y")
  date_range_to_conv <- lubridate::ymd(date_range_to) %>% format("%d%m%Y")
  
  robj <- call_api_once(
    URL = "http://ws.vlpar.be/e/opendata/",
    path = "/verg/vorige",
    query = list(type = plen_comm, dagen = 999999, limiet = 999999, datumvan = date_range_from_conv, datumtot = date_range_to_conv)
  )
  
  iterator <- robj$items$vergadering$id
  
  if (is.null(iterator)) {
    stop("No sessions found between ", date_range_from, " and ", date_range_to, ".")
  }
  
  message("Found ", length(iterator), " sessions between ", date_range_from, " and ", paste0(date_range_to, "."))
  message("Getting the session details.")
  mainlist <- call_api_multiple_times(
    iterator = iterator,
    URL = "http://ws.vlpar.be/e/opendata/",
    path = "verg",
    query = list(aanpassingen = "nee", idPrsHighlight = 0),
    resultVector = c("vergadering"),
    use_parallel = use_parallel
  )
  if (raw == TRUE) {
    return(mainlist)
  }
  if (plen_comm == "plen") {
    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg, names_sep = "_") %>%
      tidyr::unnest_wider(verg_journaallijn, names_sep = "_") %>%
      tidyr::unnest_wider(verg_plenairehandelingen, names_sep = "_") %>%
      dplyr::select(id_verg,
                    woordelijk_verslag = verg_plenairehandelingen_pdffilewebpath,
                    journaallijn_id = verg_journaallijn_id,
                    debatten = verg_journaallijn_debat,
                    gedachtenwisselingen = verg_journaallijn_gedachtewisseling,
                    vragen_interpelaties = verg_journaallijn_vrageninterpellatie,
                    parlementaire_initiatieven = `verg_journaallijn_parlementair-initiatief`,
                    verzoekschrift = verg_journaallijn_verzoekschrift
      ) %>%
      tidyr::unnest(cols = c(
        journaallijn_id, debatten, gedachtenwisselingen, vragen_interpelaties,
        parlementaire_initiatieven, verzoekschrift
      ), keep_empty = TRUE) %>%
      tidyr::pivot_longer(cols = c(
        debatten,
        gedachtenwisselingen,
        vragen_interpelaties,
        parlementaire_initiatieven,
        verzoekschrift
      ), names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value, names_sep = "_") %>%
      dplyr::filter(!is.na(value_id)) %>%
      tidyr::unnest(cols = c(value_id, value_contacttype, value_link, value_objecttype, value_document, value_filewebpath), names_sep = "_", keep_empty = TRUE) %>%
      tidyr::unnest_wider(value_link, names_sep = "_") %>%
      dplyr::select(id_verg,
                    id_fact = value_id,
                    fact_link = value_link_href,
                    type_activiteit,
                    type_specifiek = value_objecttype_naam,
                    woordelijk_verslag,
                    naam = value_document_bestandsnaam,
                    document = value_document_url,
                    journaallijn_id
      ) %>%
      dplyr::distinct() %>%
      tidyr::unnest(fact_link, keep_empty = TRUE) -> result_joined
    
    if (extra_via_fact == TRUE & type == "document") {
      message("Getting the extra details by checking the fact endpoints.")
      result_joined %>%
        dplyr::select(fact_link) %>%
        dplyr::mutate(verslag = stringr::str_extract(fact_link, "verslag")) %>%
        dplyr::filter(is.na(verslag)) %>%
        dplyr::select(-verslag) %>%
        dplyr::mutate(id = stringr::str_extract(fact_link, "[0-9]+")) %>%
        dplyr::mutate(url = stringr::str_extract(fact_link, "[^0-9]+")) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(url)) -> list
      
      result <- call_api_multiple_times(
        iterator = list$id,
        URL = list$url,
        path = NULL,
        query = list(),
        resultVector = NULL,
        use_parallel = use_parallel
      )
      
      result %>%
        tibble::tibble(result = ., id_fact = names(result)) %>%
        tidyr::unnest_wider(result, names_sep = "_") %>%
        tidyr::unnest(`result_parlementair-initiatief`, names_sep = "_", keep_empty = TRUE) %>%
        dplyr::select(id_fact, `result_parlementair-initiatief_document`) %>%
        tidyr::unnest(`result_parlementair-initiatief_document`, names_sep = "_", keep_empty = TRUE) %>%
        dplyr::rename(document = `result_parlementair-initiatief_document_url`) %>%
        dplyr::filter(!is.na(document)) %>%
        dplyr::left_join(result_joined %>%
                           dplyr::select(id_verg, fact_link) %>%
                           dplyr::mutate(id_fact = stringr::str_extract(fact_link, "[0-9]+")) %>%
                           dplyr::select(id_verg, id_fact) %>%
                           dplyr::distinct(), by = c("id_fact" = "id_fact")) %>%
        dplyr::select(id_verg, id_fact, docs_document = document, docs_naam = `result_parlementair-initiatief_document_bestandsnaam`) %>%
        dplyr::distinct() %>%
        dplyr::mutate(id_fact = as.numeric(id_fact)) %>%
        dplyr::filter(!docs_document %in% result_joined$main_document) -> docs
      
      result_joined %>%
        dplyr::rename(
          result_document = document,
          result_naam = naam
        ) %>%
        dplyr::left_join(docs, by = c("id_verg" = "id_verg", "id_fact" = "id_fact")) %>%
        tidyr::pivot_longer(
          cols = c("result_document", "result_naam", "docs_document", "docs_naam"),
          names_to = c("Var", ".value"),
          names_sep = "_"
        ) %>%
        dplyr::select(
          id_verg,
          id_fact,
          fact_link,
          type_activiteit,
          type_specifiek,
          woordelijk_verslag,
          journaallijn_id,
          document,
          naam
        ) %>%
        dplyr::distinct() -> result_joined
    }
    return(result_joined)
  }
  
  if (plen_comm == "comm") {
    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg, names_sep = "_") %>%
      tidyr::unnest(`verg_agenda-item`, names_sep = "_", keep_empty = TRUE) %>%
      tidyr::unnest_wider(`verg_agenda-item_agenda-lijn`) %>%
      guarantee_field("verg_commissiehandelingen") %>%
      dplyr::select(id_verg,
                    debatten = debat,
                    gedachtenwisselingen = gedachtewisseling,
                    vragen_interpelaties = vrageninterpellatie,
                    parlementaire_initiatieven = `parlementair-initiatief`,
                    verzoekschrift = verzoekschrift,
                    verg_commissiehandelingen
      ) %>%
      tidyr::unnest(cols = c(
        debatten, gedachtenwisselingen, vragen_interpelaties,
        parlementaire_initiatieven, verzoekschrift
      ), keep_empty = TRUE) %>%
      tidyr::pivot_longer(cols = c(
        debatten,
        gedachtenwisselingen,
        vragen_interpelaties,
        parlementaire_initiatieven,
        verzoekschrift
      ), names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value, names_sep = "_") %>%
      guarantee_field("value_id") %>%
      dplyr::filter(!is.na(value_id)) %>%
      guarantee_field(c("filewebpath", "value_contacttype", "value_document", "value_link", "value_objecttype", "value_onderwerp", "value_titel", "value_filewebpath", "value_zittingsjaar")) %>%
      tidyr::unnest(cols = c(value_id, value_contacttype, value_document, value_link, value_objecttype, value_onderwerp, value_titel, value_filewebpath, value_zittingsjaar), names_sep = "_", keep_empty = TRUE) %>%
      tidyr::unnest_wider(value_link, names_sep = "_") %>%
      tidyr::unnest_wider(verg_commissiehandelingen, names_sep = "_") %>%
      guarantee_field(c("verg_commissiehandelingen_pdffilewebpath", "value_link_href", "value_objecttype_naam", "value_document_bestandsnaam")) %>%
      dplyr::select(id_verg,
                    id_fact = value_id,
                    fact_link = value_link_href,
                    type_activiteit,
                    type_specifiek = value_objecttype_naam,
                    woordelijk_verslag = verg_commissiehandelingen_pdffilewebpath,
                    hoofd_document = value_filewebpath,
                    hoofd_naam = value_document_bestandsnaam
      ) %>%
      dplyr::distinct() %>%
      tidyr::unnest(fact_link, keep_empty = TRUE) -> result
    
    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg, names_sep = "_") %>%
      guarantee_field("verg_journaallijn") %>%
      tidyr::unnest_wider(verg_journaallijn, names_sep = "_") %>%
      guarantee_field(c(
        "verg_journaallijn_id", "verg_journaallijn_debat", "verg_journaallijn_gedachtewisseling",
        "verg_journaallijn_vrageninterpellatie", "verg_journaallijn_parlementair-initiatief",
        "verg_journaallijn_verzoekschrift"
      )) %>%
      dplyr::select(id_verg,
                    journaallijn_id = verg_journaallijn_id,
                    debatten = verg_journaallijn_debat,
                    gedachtenwisselingen = verg_journaallijn_gedachtewisseling,
                    vragen_interpelaties = verg_journaallijn_vrageninterpellatie,
                    parlementaire_initiatieven = `verg_journaallijn_parlementair-initiatief`,
                    verzoekschrift = verg_journaallijn_verzoekschrift
      ) %>%
      tidyr::unnest(cols = c(
        journaallijn_id, debatten, gedachtenwisselingen, vragen_interpelaties,
        parlementaire_initiatieven, verzoekschrift
      ), keep_empty = TRUE, names_sep = "_") %>%
      tidyr::pivot_longer(cols = c(
        debatten,
        gedachtenwisselingen,
        vragen_interpelaties,
        parlementaire_initiatieven,
        verzoekschrift
      ), names_to = "type_activiteit", values_to = "value") %>%
      tidyr::unnest_wider(value, names_sep = "_") %>%
      guarantee_field(c("value_id", "value_link")) %>%
      dplyr::filter(!is.na(value_id)) %>%
      tidyr::unnest(cols = c(value_id, value_link), names_sep = "_", keep_empty = TRUE) %>%
      tidyr::unnest_wider(value_link, names_sep = "_") %>%
      dplyr::select(id_verg,
                    journaallijn_id,
                    id_fact = value_id
      ) %>%
      dplyr::distinct() -> jln
    
    mainlist %>%
      tibble::tibble(verg = ., id_verg = names(mainlist)) %>%
      tidyr::unnest_wider(verg) %>%
      dplyr::select(id_verg, commissieverslag, id) %>%
      tidyr::unnest_longer(commissieverslag) %>%
      tidyr::unnest(cols = c(commissieverslag), keep_empty = TRUE) %>%
      guarantee_field(c("initiatief", "verslag", "hoorzitting_gedachtenwisseling")) %>%
      tidyr::unnest(cols = c(initiatief, verslag, hoorzitting_gedachtenwisseling), keep_empty = TRUE, names_sep = "_") %>%
      guarantee_field(c("initiatief_id", "hoorzitting_gedachtenwisseling_id", "verslag_filewebpath", "verslag_bijlage", "verslag_document")) %>%
      dplyr::select(id_verg, initiatief_id, hoorzitting_gedachtenwisseling_id, verslag_filewebpath, verslag_bijlage, verslag_document) %>%
      tidyr::unnest_wider("verslag_bijlage", names_sep = "_") %>%
      guarantee_field(c("verslag_bijlage_url", "verslag_bijlage_bestandsnaam")) %>%
      tidyr::unnest_longer(c("verslag_bijlage_url", "verslag_bijlage_bestandsnaam")) %>%
      tidyr::unnest(cols = c(verslag_document), keep_empty = TRUE) %>%
      guarantee_field(c("initiatief_id", "hoorzitting_gedachtenwisseling_id", "verslag_filewebpath", "bestandsnaam", "verslag_bijlage_url", "verslag_bijlage_bestandsnaam")) %>%
      dplyr::select(id_verg,
                    ini_fact = initiatief_id,
                    hg_fact = hoorzitting_gedachtenwisseling_id,
                    ini_document = verslag_filewebpath,
                    ini_naam = bestandsnaam,
                    hg_document = verslag_bijlage_url,
                    hg_naam = verslag_bijlage_bestandsnaam
      ) %>%
      dplyr::filter(!is.na(ini_fact) | !is.na(hg_fact)) %>%
      tidyr::pivot_longer(
        cols = ini_document:hg_naam,
        names_to = c("Var", ".value"),
        names_sep = "_"
      ) %>%
      tidyr::pivot_longer(
        cols = c("ini_fact", "hg_fact"),
        values_to = "id_fact"
      ) %>%
      dplyr::select(id_verg, id_fact, verslag_document = document, verslag_naam = naam) %>%
      na.omit() %>%
      dplyr::distinct() -> verslag
    
    result %>%
      dplyr::left_join(verslag, by = c("id_verg" = "id_verg", "id_fact" = "id_fact")) %>%
      tidyr::pivot_longer(
        cols = hoofd_document:verslag_naam,
        names_to = c("Var", ".value"),
        names_sep = "_"
      ) %>%
      dplyr::left_join(jln, by = c("id_verg" = "id_verg", "id_fact" = "id_fact")) %>%
      dplyr::distinct() %>%
      dplyr::select(
        id_verg,
        id_fact,
        fact_link,
        type_activiteit,
        type_specifiek,
        woordelijk_verslag,
        document,
        naam,
        journaallijn_id
      ) %>%
      dplyr::distinct() -> result_joined
    
    if (nrow(result_joined) == 0) {
      message("Query resulted in empty dataframe, usually means nothing was found, try to broaden your search query.")
    }
    
    if (extra_via_fact == TRUE & type == "document") {
      message("Getting the extra details by checking the fact endpoints.")
      result_joined %>%
        dplyr::select(fact_link) %>%
        dplyr::mutate(verslag = stringr::str_extract(fact_link, "verslag")) %>%
        dplyr::filter(is.na(verslag)) %>%
        dplyr::select(-verslag) %>%
        dplyr::mutate(id = stringr::str_extract(fact_link, "[0-9]+")) %>%
        dplyr::mutate(url = stringr::str_extract(fact_link, "[^0-9]+")) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(url)) -> list
      
      result <- call_api_multiple_times(
        iterator = list$id,
        URL = list$url,
        path = NULL,
        query = list(),
        resultVector = NULL,
        use_parallel = use_parallel
      )
      
      result %>%
        tibble::tibble(result = ., id_fact = names(result)) %>%
        tidyr::unnest_wider(result, names_sep = "_") %>%
        tidyr::unnest(`result_parlementair-initiatief`, names_sep = "_", keep_empty = TRUE) %>%
        dplyr::select(id_fact, `result_parlementair-initiatief_document`) %>%
        tidyr::unnest(`result_parlementair-initiatief_document`, names_sep = "_", keep_empty = TRUE) %>%
        dplyr::rename(document = `result_parlementair-initiatief_document_url`) %>%
        dplyr::filter(!is.na(document)) %>%
        dplyr::left_join(result_joined %>%
                           dplyr::select(id_verg, fact_link) %>%
                           dplyr::mutate(id_fact = stringr::str_extract(fact_link, "[0-9]+")) %>%
                           dplyr::select(id_verg, id_fact) %>%
                           dplyr::distinct(), by = c("id_fact" = "id_fact")) %>%
        dplyr::select(id_verg, id_fact, docs_document = document, docs_naam = `result_parlementair-initiatief_document_bestandsnaam`) %>%
        dplyr::distinct() %>%
        dplyr::mutate(id_fact = as.numeric(id_fact)) %>%
        dplyr::filter(!docs_document %in% result_joined$document) -> docs
      
      result_joined %>%
        dplyr::rename(
          result_document = document,
          result_naam = naam
        ) %>%
        dplyr::left_join(docs, by = c("id_verg" = "id_verg", "id_fact" = "id_fact")) %>%
        tidyr::pivot_longer(
          cols = c("result_document", "result_naam", "docs_document", "docs_naam"),
          names_to = c("Var", ".value"),
          names_sep = "_"
        ) %>%
        dplyr::select(
          id_verg,
          id_fact,
          fact_link,
          type_activiteit,
          type_specifiek,
          woordelijk_verslag,
          journaallijn_id,
          document,
          naam
        ) %>%
        dplyr::distinct() -> result_joined
    }
  }
  
  return(result_joined)
}


#' Get the speech of either plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search.
#' @param plen_comm Switch between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @param type Type of data to be returned, options include "document", "speech" or "details".
#' @importFrom dplyr %>%
get_plen_comm_speech <- function(date_range_from, date_range_to, fact, plen_comm = "plen", type = "details", use_parallel = TRUE, raw = FALSE) {
  session_object <- get_sessions_details(
    date_range_from = date_range_from,
    date_range_to = date_range_to,
    use_parallel = use_parallel,
    plen_comm = plen_comm,
    type = type
  )
  session_object %>%
    dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
    dplyr::filter(type_eng %in% fact) %>%
    dplyr::filter(!is.na(journaallijn_id)) -> session_object
  
  if (length(session_object$id_verg) == 0) {
    stop("No facts found. Usually this means the type of fact you are looking for did not occur during this time. Or that this fact does not contain spoken data.")
  }
  
  message("Getting speech.")
  
  result <- call_api_multiple_times(
    iterator = unique(session_object$journaallijn_id),
    URL = "http://ws.vlpar.be/e/opendata/",
    path = "jln",
    query = list(),
    resultVector = NULL,
    use_parallel = use_parallel
  )
  
  if (raw == TRUE) {
    return(result)
  }
  
  tibble::tibble(col = result, journaallijn_id = names(result)) %>%
    tidyr::unnest_wider(col) %>%
    tidyr::unnest_wider(vrageninterpellatie, names_sep = "_") %>%
    guarantee_field(c("vrageninterpellatie_id")) %>%
    tidyr::unnest(c(vrageninterpellatie_id), keep_empty = TRUE) %>%
    tidyr::unnest_wider("parlementair-initiatief", names_sep = "_") %>%
    guarantee_field(c("parlementair-initiatief_id", "debat")) %>%
    tidyr::unnest(c("parlementair-initiatief_id"), keep_empty = TRUE) %>%
    tidyr::unnest_wider(c("debat"), names_sep = "_") %>%
    tidyr::unnest_wider(spreker, names_sep = "_") %>%
    guarantee_field("spreker_persoon") %>%
    tidyr::unnest_wider(spreker_persoon, names_sep = "_") %>%
    guarantee_field(c("spreker_sprekertekst", "spreker_sprekertitel", "spreker_persoon_id", "debat_id")) %>%
    dplyr::select(vrageninterpellatie_id,
                  `parlementair-initiatief_id`,
                  debat_id,
                  journaallijn_id,
                  text = spreker_sprekertekst,
                  sprekertitel = spreker_sprekertitel,
                  persoon_id = spreker_persoon_id
    ) %>%
    dplyr::mutate(id_fact = ifelse(!is.na(vrageninterpellatie_id), vrageninterpellatie_id,
                                   ifelse(!is.na(`parlementair-initiatief_id`), `parlementair-initiatief_id`,
                                          ifelse(!is.na(debat_id), debat_id)
                                   )
    )) %>%
    dplyr::select(-vrageninterpellatie_id, -`parlementair-initiatief_id`, -debat_id) %>%
    tidyr::unnest(cols = c(text, sprekertitel, persoon_id), keep_empty = TRUE) %>%
    dplyr::mutate(journaallijn_id = as.character(journaallijn_id)) %>%
    dplyr::mutate(id_fact = as.character(id_fact)) -> raw_text_spoken
}


#' Get the details from plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search
#' @param type Type of data to be returned, options include "document", "speech" or "details".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @param extra_via_fact Boolean: also search the underlying endpoint for linked documents? This may return documents not linked to the specific meeting, thus may also include meetings on dates before/after the date range.
#' @importFrom dplyr %>%
get_plen_comm_details <- function(date_range_from, date_range_to, fact, plen_comm = "plen", type = "details", use_parallel = TRUE, raw = FALSE, extra_via_fact = FALSE) {
  session_object <- get_sessions_details(
    date_range_from = date_range_from,
    date_range_to = date_range_to,
    use_parallel = use_parallel,
    plen_comm = plen_comm,
    type = type,
    extra_via_fact = extra_via_fact
  )
  
  session_object %>%
    dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
    dplyr::filter(type_eng %in% fact) %>%
    dplyr::select(fact_link) %>%
    dplyr::mutate(id = stringr::str_extract(fact_link, "[0-9]+")) %>%
    dplyr::mutate(url = stringr::str_extract(fact_link, "[^0-9]+")) %>%
    dplyr::select(-fact_link) %>%
    dplyr::distinct() -> mainlist
  
  if (length(mainlist$id) == 0) {
    stop("No facts found. Usually this means the type of fact you are looking for did not occur during the specified time.")
  }
  
  message("Getting and parsing the details.")
  
  list <- call_api_multiple_times(
    iterator = mainlist$id,
    URL = mainlist$url,
    path = NULL,
    query = list(),
    resultVector = NULL,
    use_parallel = use_parallel
  )
  
  if (raw == TRUE) {
    return(list)
  }
  
  if (fact == "parliamentary_initiatives") {
    if (plen_comm == "plen") {
      session_object %>%
        dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
        dplyr::filter(type_eng %in% fact) %>%
        dplyr::distinct() %>%
        dplyr::left_join(list %>%
                           tibble::tibble(result = ., id_fact = names(list)) %>%
                           tidyr::unnest_wider(result, names_sep = "_") %>%
                           tidyr::unnest_longer(result_journaallijn) %>%
                           tidyr::unnest(result_journaallijn, names_sep = "_") %>%
                           tidyr::unnest(result_journaallijn_vergadering, names_sep = "_") %>%
                           tidyr::unnest_wider(result_thema, names_sep = "_") %>%
                           guarantee_field(c(
                             "result_contacttype",
                             "titel",
                             "onderwerp",
                             "zittingsjaar",
                             "result_journaallijn-stemmingen",
                             "result_numac",
                             "result_nummer",
                             "result_staatsblad",
                             "result_kruispuntbank-url"
                           )) %>%
                           dplyr::select(
                             id_fact = result_id,
                             id_verg = result_journaallijn_vergadering_id,
                             session_start_date = result_journaallijn_vergadering_datumbegin,
                             session_end_date = result_journaallijn_vergadering_datumeinde,
                             contacttype = result_contacttype,
                             titel = result_titel,
                             onderwerp = result_onderwerp,
                             materie = result_materie,
                             zittingsjaar = result_zittingsjaar,
                             vote = `result_journaallijn-stemmingen`,
                             extra_details = `result_parlementair-initiatief`,
                             numac = result_numac,
                             nr = result_nummer,
                             staatsblad = result_staatsblad,
                             kruispuntbank_url = `result_kruispuntbank-url`,
                             status = result_status,
                             dplyr::starts_with("result_thema")
                           ) %>%
                           tidyr::unnest_wider(staatsblad, names_sep = "_") %>%
                           tidyr::unnest_wider(vote, names_sep = "_") %>%
                           guarantee_field(c("vote_stemming")) %>%
                           tidyr::unnest(vote_stemming, names_sep = "_") %>%
                           dplyr::distinct() %>%
                           dplyr::mutate(id_verg = as.character(id_verg)), by = c("id_fact" = "id_fact", "id_verg" = "id_verg")) %>%
        dplyr::distinct() %>%
        dplyr::select(id_verg, id_fact, journaallijn_id, dplyr::everything()) -> result
    }
    if (plen_comm == "comm") {
      session_object %>%
        dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
        dplyr::filter(type_eng %in% fact) %>%
        dplyr::distinct() %>%
        dplyr::left_join(list %>%
                           tibble::tibble(result = ., id_fact = names(list)) %>%
                           tidyr::unnest_wider(result, names_sep = "_") %>%
                           tidyr::unnest_longer(result_commissiegroepering) %>%
                           tidyr::unnest(result_commissiegroepering, names_sep = "_") %>%
                           guarantee_field(c(
                             "result_contacttype",
                             "result_titel",
                             "result_onderwerp",
                             "result_zittingsjaar"
                           )) %>%
                           tidyr::unnest(result_commissiegroepering_vergadering, names_sep = "_") %>%
                           tidyr::unnest_wider(result_thema, names_sep = "_") %>%
                           tidyr::unnest(result_commissiegroepering_vergadering_commissie, names_sep = "_") %>%
                           dplyr::select(id_fact,
                                         id_verg = result_commissiegroepering_vergadering_id,
                                         id_comm = result_commissiegroepering_vergadering_commissie_id,
                                         comm_title = result_commissiegroepering_vergadering_commissie_titel,
                                         session_start_date = result_commissiegroepering_vergadering_datumbegin,
                                         session_end_date = result_commissiegroepering_vergadering_datumeinde,
                                         zittingsjaar = result_zittingsjaar,
                                         onderwerp = result_onderwerp,
                                         titel = result_titel,
                                         status = result_status,
                                         materie = result_materie,
                                         result_contacttype,
                                         extra_details = `result_parlementair-initiatief`,
                                         dplyr::starts_with("result_thema")
                           ) %>%
                           dplyr::distinct() %>%
                           dplyr::mutate(
                             id_verg = as.character(id_verg),
                             id_fact = as.integer(id_fact)
                           ), by = c("id_fact" = "id_fact", "id_verg" = "id_verg")) %>%
        dplyr::distinct() %>%
        dplyr::select(id_verg, id_fact, journaallijn_id, dplyr::everything()) -> result
    }
    return(result)
  }
  
  if (fact == "debates") {
    session_object %>%
      dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
      dplyr::filter(type_eng %in% fact) %>%
      dplyr::distinct() %>%
      dplyr::left_join(list %>%
                         tibble::tibble(result = ., id_fact = names(list)) %>%
                         tidyr::unnest_wider(result, names_sep = "_") %>%
                         tidyr::unnest_wider(result_thema, names_sep = "_") %>%
                         tidyr::unnest_wider(result_journaallijn, names_sep = "_") %>%
                         tidyr::unnest_wider(result_journaallijn_vergadering, names_sep = "_") %>%
                         dplyr::select(id_fact,
                                       id_verg = result_journaallijn_vergadering_id,
                                       titel = result_titel,
                                       onderwerp = result_onderwerp,
                                       session_start_date = result_journaallijn_vergadering_datumbegin,
                                       session_end_date = result_journaallijn_vergadering_datumeinde,
                                       zittingsjaar = result_zittingsjaar,
                                       status = result_status,
                                       contacttype = result_contacttype,
                                       sprekers = result_spreker,
                                       dplyr::starts_with("result_thema")
                                       # ,result_procedureverloop
                         ) %>%
                         tidyr::unnest(id_verg) %>%
                         dplyr::distinct() %>%
                         dplyr::mutate(
                           id_verg = as.character(id_verg),
                           id_fact = as.integer(id_fact)
                         ), by = c("id_fact" = "id_fact", "id_verg" = "id_verg")) -> result
    return(result)
  }
  
  if (fact == "oral_questions_and_interpellations") {
    list %>%
      tibble::tibble(result = ., id_fact = names(list)) %>%
      tidyr::unnest_wider(result, names_sep = "_") %>%
      tidyr::unnest_wider(result_thema, names_sep = "_") %>%
      tidyr::unnest(c(result_journaallijn), keep_empty = TRUE) %>%
      guarantee_field(c("vergadering")) %>%
      tidyr::unnest(c(vergadering), names_sep = "_", keep_empty = TRUE) %>%
      guarantee_field(c("result_commissie", "vergadering_type", "vergadering_subtype")) %>%
      tidyr::unnest_wider(result_commissie, names_sep = "_") %>%
      guarantee_field(c("id",
                        "result_commissie_id",
                        "result_commissie_titel",
                        "result_commissie_afkorting",
                        "vergadering_datumagendering",
                        "vergadering_datumbegin",
                        "vergadering_datumeinde")) %>%
      dplyr::select(
        verg_id = id,
        id_fact,
        journaallijn_id = id,
        titel = result_titel,
        onderwerp = result_onderwerp,
        zittingsjaar = result_zittingsjaar,
        agenda_date = vergadering_datumagendering,
        session_date_start = vergadering_datumbegin,
        session_date_end = vergadering_datumeinde,
        commissie_id = result_commissie_id,
        commissie_titel = result_commissie_titel,
        commissie_afkorting = result_commissie_afkorting,
        type = vergadering_type,
        subtype = vergadering_subtype,
        result_contacttype,
        dplyr::starts_with("result_thema")
      ) %>%
      dplyr::mutate(journaallijn_id = as.character(journaallijn_id)) -> result
    return(result)
  }
  
  if (fact == "committee_hearings") {
    session_object %>%
      dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
      dplyr::filter(type_eng %in% fact) %>%
      dplyr::distinct() %>%
      dplyr::rename(naam_document = naam) %>%
      dplyr::filter(!is.na(document)) %>%
      dplyr::left_join(list %>%
                         tibble::tibble(result = ., id_fact = names(list)) %>%
                         tidyr::unnest_wider(result, names_sep = "_") %>%
                         tidyr::unnest_wider(result_thema, names_sep = "_") %>%
                         tidyr::unnest_wider(result_procedureverloop, names_sep = "_") %>%
                         tidyr::unnest(cols = c(result_procedureverloop_vergadering), keep_empty = TRUE) %>%
                         dplyr::select(id_fact,
                                       id_verg = id,
                                       session_start_date = datumbegin,
                                       session_end_date = datumeinde,
                                       omschrijving,
                                       titel = result_titel,
                                       onderwerp = result_onderwerp,
                                       dplyr::starts_with("result_thema"),
                                       zittingsjaar = result_zittingsjaar,
                                       status = result_status
                                       # ,result_objecttype
                                       , result_contacttype,
                                       `result_parlementair-initiatief`,
                                       base_initiatives = `result_basis-initiatieven`
                         ) %>%
                         dplyr::filter(!is.na(id_verg)) %>%
                         tidyr::hoist(result_contacttype,
                                      naam = list("contact", 1, "naam"),
                                      voornaam = list("contact", 1, "voornaam"),
                                      role = list("beschrijving"),
                                      id_mp = list("contact", 1, "id")
                         ) %>%
                         dplyr::distinct() %>%
                         dplyr::mutate(
                           id_verg = as.character(id_verg),
                           id_fact = as.integer(id_fact)
                         ), by = c("id_fact" = "id_fact", "id_verg" = "id_verg")) %>%
      dplyr::mutate(omschrijving = gsub("<(.|\n)*?>", "", omschrijving)) -> result
    return(result)
  }
  
  # if(fact=="verzoekschriften"){
  #
  #   result %>%
  #     tibble::tibble(result = ., id_fact = names(result)) %>%
  #     tidyr::unnest_wider(result,names_sep="_") %>%
  #     tidyr::unnest_wider(result_thema,names_sep="_") %>%
  #
  #     dplyr::select(id_fact
  #                   ,result_titel
  #                   ,result_onderwerp
  #                   ,dplyr::starts_with("result_thema")
  #                   ,result_zittingsjaar
  #                   ,result_status
  #                   ,result_objecttype
  #                   ,result_contacttype
  #                   ,`result_parlementair-initiatief`
  #                   ,result_samenhang
  #                   ,result_procedureverloop   ) %>%
  #     tidyr::hoist(result_contacttype,
  #                  naam = list("contact", 1, "naam"),
  #                  voornaam = list("contact", 1, "voornaam"),
  #                  role= list( "beschrijving"),
  #                  id_mp = list("contact", 1, "id")) -> result
  #
  #   return(result)
  #
  # }
}


#' Get the documents discussed in plenary or commission sessions
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact The fact to search.
#' @param type Type of data to be returned, options include "document", "speech" or "details".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @param extra_via_fact Boolean: also search the underlying endpoint for linked documents? This may return documents not linked to the specific meeting, thus may also include meetings on dates before/after the date range.
#' @importFrom dplyr %>%
get_plen_comm_documents <- function(date_range_from, date_range_to, fact, plen_comm = "plen", type = "details", use_parallel = TRUE, raw = FALSE, extra_via_fact = FALSE) {
  session_object <- get_sessions_details(
    date_range_from = date_range_from,
    date_range_to = date_range_to,
    use_parallel = use_parallel,
    plen_comm = plen_comm,
    type = type,
    extra_via_fact = extra_via_fact
  )
  session_object %>%
    dplyr::left_join(type_conv, by = c("type_activiteit" = "type_nl")) %>%
    dplyr::filter(type_eng %in% fact) %>%
    dplyr::filter(!is.na(document)) %>%
    dplyr::mutate(id = stringr::str_extract(document, "[0-9]+")) %>%
    dplyr::select(id_verg, id_fact, id, document) %>%
    dplyr::distinct() -> mainlist
  
  if (length(mainlist$document) == 0) {
    stop("No facts found. Usually this means the type of fact you are looking for did not occur during the specified time or that the specified output is not present.")
  }
  
  message("Getting and parsing the documents.")
  list <- parse_documents(mainlist = mainlist, use_parallel = use_parallel)
  tibble::tibble(list = list) %>%
    tidyr::unnest(list, keep_empty = TRUE) -> result
  
  return(result)
}


#' Get data from the Flemish parliament
#'
#' @param date_range_from The start date, should be in format "yyyy-mm-dd".
#' @param date_range_to The end date, should be in format "yyyy-mm-dd".
#' @param fact Which fact should be returned, options include "written_questions", "debates", "oral_questions_and_interpellations", "parliamentary_initiatives" or "committee_hearings"
#' @param type Type of data to be returned, options include "document", "speech" or "details".
#' @param plen_comm Switch to pick between plenary (plen) and commission (comm) sessions.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @param raw Boolean: should the raw object be returned?
#' @param extra_via_fact Boolean: also search the underlying endpoint for linked documents? This may return documents not linked to the specific meeting, thus may also include meetings on dates before/after the date range.
#' @param two_columns_pdf Boolean: Use when you encounter two-colummned PDFs.
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#'
#' get_work(
#'   date_range_from = "2022-01-20",
#'   date_range_to = "2022-01-31",
#'   type = "document",
#'   fact = "written_questions",
#'   use_parallel = TRUE
#' )
#' }
get_work <- function(date_range_from, date_range_to, fact = "debates", type = "details", plen_comm = "plen", use_parallel = TRUE, raw = FALSE, extra_via_fact = FALSE, two_columns_pdf = FALSE) {
  type_list <- c("document", "speech", "details")
  facts_list <- c("written_questions", "debates", "oral_questions_and_interpellations", "parliamentary_initiatives", "committee_hearings")
  
  # types
  if (!any(type_list %in% type)) {
    stop(paste0("Supply valid type. Valid options are ", toString(tolower(type_list)), ". Select one type."))
  }
  if (length(type) != 1) {
    stop("You have selected multiple type options. Please set type to ", toString(tolower(facts)), ".")
  }
  # facts
  if (!any(facts_list %in% fact)) {
    stop(paste0("Supply valid type. Valid options are ", toString(tolower(facts_list)), ". Select one type."))
  }
  if (length(fact) != 1) {
    stop("You have selected multiple fact options. Please set type to ", toString(tolower(facts_list)), ".")
  }
  if ("plen" %in% plen_comm & "committee_hearings" %in% fact) {
    stop("You have selected an incompatible combination (Council hearings are not held in plenary sessions).  To see all the option type `possibilities` in the console.")
  }
  if ("comm" %in% plen_comm & "debates" %in% fact) {
    stop("You have selected an incompatible combination (Debates are not held in plenary sessions).  To see all the option type `possibilities` in the console.")
  }
  if ("plen" %in% plen_comm & "debates" %in% fact & type == "document" ) {
    stop("You have selected an incompatible combination (No documents are associated with a debate). To see all the option type `possibilities` in the console.")
  }
  if ("oral_questions_and_interpellations" %in% fact & type == "document") {
    stop("You have selected an incompatible combination (Documents are not associated with oral questions and interpellations).  To see all the option type `possibilities` in the console.")
  }
  if ("written_questions" %in% fact & type == "speech" ) {
    stop("You have selected an incompatible combination (No speech is associated with a written question). To see all the option type `possibilities` in the console.")
  }
  # written questions
  if ("document" %in% type & "written_questions" %in% fact) {
    object <- get_written_questions_documents(
      date_range_from = date_range_from,
      date_range_to = date_range_to,
      use_parallel = use_parallel,
      two_columns_pdf = two_columns_pdf
    )
    object %>%
      dplyr::select(id_fact, publicatiedatum, text) %>%
      as.data.frame() -> result
    
    return(result)
  }
  
  # written questions
  if ("details" %in% type & "written_questions" %in% fact) {
    object <- get_written_questions_details(
      date_range_from = date_range_from,
      date_range_to = date_range_to,
      use_parallel = use_parallel,
      raw = raw
    )
    return(object)
  }
  
  # get speech from "debates","oral_questions_and_interpellations","parliamentary_initiatives","committee_hearings"
  if ("speech" %in% type & any(facts_list %in% fact)) {
    object <- get_plen_comm_speech(
      date_range_from = date_range_from,
      date_range_to = date_range_to,
      plen_comm = plen_comm,
      fact = fact,
      use_parallel = use_parallel,
      raw = raw,
      type = type
    )
    return(object)
  }
  
  # get details from "debates","oral_questions_and_interpellations","parliamentary_initiatives","committee_hearings"
  if ("details" %in% type & any(facts_list %in% fact)) {
    object <- get_plen_comm_details(
      date_range_from = date_range_from,
      date_range_to = date_range_to,
      plen_comm = plen_comm,
      fact = fact,
      use_parallel = use_parallel,
      raw = raw,
      type = type,
      extra_via_fact = extra_via_fact
    )
    return(object)
  }
  
  # get documents from "debates","oral_questions_and_interpellations","parliamentary_initiatives","committee_hearings"
  if ("document" %in% type & any(facts_list %in% fact)) {
    object <- get_plen_comm_documents(
      date_range_from = date_range_from,
      date_range_to = date_range_to,
      plen_comm = plen_comm,
      fact = fact,
      use_parallel = use_parallel,
      raw = raw,
      type = type,
      extra_via_fact = extra_via_fact
    )
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
#' \dontrun{
#'
#' wq_document <- get_work(
#'   date_range_from = "2022-01-20",
#'   date_range_to = "2022-01-31",
#'   type = "document",
#'   fact = "written_questions",
#'   use_parallel = TRUE
#' )
#'
#' search_terms(wq_document, text_field = "text", search_terms = "PFOS")
#' }
search_terms <- function(df, text_field, search_terms = NULL) {
  message("Scrubbing away all html-tags ")
  
  df %>%
    tibble::as_tibble() %>%
    dplyr::rename(text_col = !!text_field) %>%
    dplyr::mutate(text_col = gsub("\r", "", text_col)) %>%
    dplyr::mutate(text_col = gsub("\n", "", text_col)) -> raw_text
  
  for (i in seq_along(raw_text$text_col)) {
    raw_text$text_col[[i]] <- xml2::xml_text(xml2::read_html(charToRaw(raw_text$text_col[[i]])))
  }
  
  raw_text %>%
    dplyr::filter(stringr::str_detect(tolower(text_col), gsub(", ", "|", toString(tolower(search_terms))))) -> result
  
  return(result)
}


#' Search all the MPs serving, or who served the Flemish parliament
#'
#' @param selection Select either "current", at a certain date "date" or "former".
#' @param date_at When selecting "date" in selection, provide the date, using the following format "yyyy-mm-dd".
#' @param fact Options of facts to return are 'raw', 'bio', 'education', 'career','political_info', 'presences_commissions' or 'presences_plenary'.
#' @param use_parallel Boolean: should parallel workers be used to call the API?
#' @export
#'
#' @examples
#' \dontrun{
#'
#' get_mp(use_parallel = TRUE, fact = "bio", date_at = "1998-01-01", selection = "date")
#' }
get_mp <- function(selection = "current", fact = "bio", date_at = NULL, use_parallel = TRUE) {
  facts <- c("raw", "bio", "education", "career", "political_info", "presences_commissions", "presences_plenary")
  
  if (length(selection) != 1) {
    stop("You have selected multiple selection options. Please set selection to either 'current', 'all' or 'date'.")
  }
  
  if (length(fact) != 1) {
    stop("You have selected multiple type options. Please set type to ", toString(tolower(facts)), ".")
  }
  
  if (any(!fact %in% facts)) {
    stop("Not a valid type. Valid options are ", toString(tolower(facts)), ". Select one type.")
  }
  
  if (selection == "current") {
    if (!is.null(date_at)) {
      message("Selection is set to current, date_at will be ignored.")
      date_at <- NULL
    }
    
    date_at_conv <- Sys.Date() %>% format("%d%m%Y")
    robj <- call_api_once(
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv/op-datum",
      query = list(datum = date_at_conv)
    )
    mainlist <- call_api_multiple_times(
      iterator = robj$items$volksvertegenwoordiger$id,
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv",
      query = list(lang = "nl"),
      resultVector = NULL,
      use_parallel = use_parallel
    )
  }
  
  # DATE
  if (selection == "date") {
    if (is.null(date_at)) {
      stop("You have selected date as selection criteria, but failed to provide a date. Please set date_at (yyyy-mm-dd).")
    }
    
    if (is.na(lubridate::ymd(date_at))) {
      stop("Wrong Date Format, please use yyyy-mm-dd.")
    }
    
    date_at_conv <- lubridate::ymd(date_at) %>% format("%d%m%Y")
    robj <- call_api_once(
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv/op-datum",
      query = list(datum = date_at_conv)
    )
    mainlist <- call_api_multiple_times(
      iterator = robj$items$volksvertegenwoordiger$id,
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv",
      query = list(lang = "nl"),
      resultVector = NULL,
      use_parallel = use_parallel
    )
  }
  
  # FORMER
  if (selection == "former") {
    if (!is.null(date_at)) {
      message("Selection is set to former, date_at will be ignored")
      date_at <- NULL
    }
    robj <- call_api_once(
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv/gewezen",
      query = list()
    )
    mainlist <- call_api_multiple_times(
      iterator = robj$items$volksvertegenwoordiger$id,
      URL = "http://ws.vlpar.be/e/opendata/",
      path = "/vv",
      query = list(lang = "nl"),
      resultVector = NULL,
      use_parallel = use_parallel
    )
  }
  
  fact <- tolower(fact)
  if (fact == "raw") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) -> result
    return(result)
  }
  
  if (fact == "bio") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("domicillieadres","email","naam","geslacht","geboortedatum","geboorteplaats", "gsmnr", "website", "huidigefractie")) %>%
      dplyr::select(id_mp = id, voornaam, achternaam = naam, geslacht, geboortedatum, geboorteplaats, gsmnr, email, website, huidigefractie) %>%
      tidyr::unnest_wider(huidigefractie) %>%
      dplyr::select(id_mp, voornaam, achternaam, geslacht, geboortedatum, geboorteplaats, gsmnr, email, website, party_id = id, party_naam = naam) %>%
      dplyr::mutate(geboortedatum = lubridate::date(lubridate::ymd_hms(geboortedatum))) %>%
      guarantee_field(c("domicillieadres")) %>%
      tidyr::unnest_wider(domicillieadres, names_sep = "_") %>%
      guarantee_field(c("domicillieadres_deelgemeente", "domicillieadres_nr", "domicillieadres_straat", "domicillieadres_telnr")) %>%
      tidyr::unnest_wider(domicillieadres_deelgemeente, names_sep = "_") %>%
      guarantee_field(c("domicillieadres_deelgemeente_naam", "domicillieadres_deelgemeente_postnr")) %>%
      dplyr::select(id_mp, voornaam, achternaam, geslacht, geboortedatum, geboorteplaats, gsmnr, email, website, party_id, party_naam ) %>%
      tidyr::unnest_wider(email, names_sep = "_") %>%
      tidyr::unnest(website, names_sep = "_", keep_empty = TRUE) %>%
      guarantee_field(c("website_soort", "website_value")) %>%
      tidyr::pivot_wider(names_from = website_soort, values_from = website_value) %>%
      dplyr::select(-`NA`) -> result
    return(result)
  }
  
  if (fact == "education") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("opleiding")) %>%
      dplyr::select(id_mp = id, voornaam, naam, opleiding) %>%
      tidyr::unnest(opleiding, keep_empty = TRUE) -> result
    return(result)
  }
  
  if (fact == "career") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("beroep")) %>%
      dplyr::select(id_mp = id, voornaam, naam, beroep) %>%
      tidyr::unnest(beroep, keep_empty = TRUE) %>%
      guarantee_field(c("datumvanformaat", "datumtotformaat")) %>%
      dplyr::select(-datumtotformaat, -datumvanformaat) %>%
      dplyr::select(id_mp, voornaam, naam, datumvan, datumtot, titel, werkgever) -> result
    return(result)
  }
  
  if (fact == "presences_commissions") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id_mp = id, voornaam, naam, aanwezigheden = `aanwezigheden-huidige-legislatuur`) %>%
      tidyr::unnest_longer(aanwezigheden) %>%
      dplyr::filter(aanwezigheden_id == "commissie-aanw") %>%
      tidyr::unnest_wider(aanwezigheden) %>%
      tidyr::unnest(c(commissie, `vast-lid-aanwezigheid`, `plaatsvervangend-lid-aanwezigheid`), names_sep = "_", keep_empty = TRUE) -> result
    return(result)
  }
  
  if (fact == "presences_plenary") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      dplyr::select(id_mp = id, voornaam, naam, aanwezigheden = `aanwezigheden-huidige-legislatuur`) %>%
      tidyr::unnest_longer(aanwezigheden) %>%
      dplyr::filter(aanwezigheden_id == "plenaire-aanw") %>%
      tidyr::unnest_wider(aanwezigheden) -> result
    return(result)
  }
  
  if (fact == "political_info") {
    mainlist %>%
      tibble::tibble(vv = .) %>%
      tidyr::unnest_wider(vv) %>%
      guarantee_field(c("interesse")) %>%
      dplyr::select(
        id_mp = id,
        voornaam, naam,
        huidigefractie,
        mandaat_vl = `mandaat-vlaams-parlement`,
        mandaat_andere = `mandaat-andere`,
        kieskring,
        deelstaatsenator,
        lidmaatschap,
        interesse
      ) %>%
      tidyr::unnest_wider(lidmaatschap, names_sep = "_") %>%
      tidyr::unnest(c(`lidmaatschap_datumVan`, `lidmaatschap_fractie`, `lidmaatschap_datumTot`), names_sep = "_", keep_empty = TRUE) %>%
      dplyr::mutate(lidmaatschap_datumVan = lubridate::date(lidmaatschap_datumVan)) %>%
      tidyr::unnest_wider(huidigefractie, names_sep = "_") %>%
      tidyr::unnest_wider(interesse, names_sep = "_") %>%
      guarantee_field(c("interesse_volgorde", "interesse_interesse")) %>%
      dplyr::select(-interesse_volgorde, -lidmaatschap_datumTot) %>%
      tidyr::unnest_wider(interesse_interesse, names_sep = "_") %>%
      dplyr::select(-huidigefractie_kleur, -huidigefractie_logo, -lidmaatschap_fractie_id, -lidmaatschap_fractie_kleur, -lidmaatschap_fractie_logo, -lidmaatschap_fractie_naam, -`lidmaatschap_fractie_zetel-aantal`) %>%
      dplyr::rename(party_id = huidigefractie_id, party_name = huidigefractie_naam, party_seats = `huidigefractie_zetel-aantal`) -> result
    return(result)
  }
}
