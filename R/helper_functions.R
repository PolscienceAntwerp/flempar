
#' Call an API once
#'
#' @param URL endpoint url
#' @param path the path
#' @param query the actual query (with list())
#' @param ... whichever to add behind path
call_api_once <- function(URL,path,query,...){

  response <- httr::GET( file.path(URL,path,...), query=query)

  if(identical(as.numeric(httr::status_code(response)),200)){

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
call_api_multiple_times <- function(iterator, URL, path, query, resultVector,use_parallel=TRUE){

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

                        call_api_once(URL,
                                      path,
                                      query,
                                      iterator[[i]]) %>%
                          purrr::pluck(!!!resultVector) %>% return

                      }#endparallel

    })#endtiming

    message(crayon::green(cli::symbol$tick,"Got the responses in", round(time_used[[3]],1), "seconds."))

    names(list) <- iterator

  }#end if

  if(use_parallel==FALSE){

    list <- vector(mode="list",length= length(iterator))

    message(crayon::green(cli::symbol$tick,"Getting the data. Take into account that dividing the tasks between workers by setting 'use_parallel=TRUE' may be much faster."))

    time_used <- system.time({

      for(i in seq_along(iterator)){

        tryCatch(

          call_api_once (URL,
                         path,
                         query,
                         iterator[[i]]) %>%
            purrr::pluck(!!!resultVector) %>% return,

          error=function(e){
            return(toString(e))
            stop()
          }
        ) ->  list[[i]]

      }

    })#endtiming

    message(crayon::green(cli::symbol$tick,"Got the responses in", round(time_used[[3]],1), "seconds."))

    names(list) <- iterator

  }#end if

  return(list)

}#endfunction

