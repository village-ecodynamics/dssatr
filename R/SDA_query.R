SDA_query <- function (q) 
{
  if (!requireNamespace("httr", quietly = TRUE) |
      !requireNamespace("jsonlite", quietly = TRUE) |
      !requireNamespace("readr", quietly = TRUE)) 
    stop("please install the `httr`, `jsonlite`, and `readr` packages", 
         call. = FALSE)
  
  df <- httr::POST(url = "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest", 
                   body = list(query = q, format = "xml") %>%
                     jsonlite::toJSON(auto_unbox = TRUE)) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text", encoding = "UTF-8") %>% 
    XML::xmlToDataFrame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::select(-element)
  
  df <- df[-1,]
  
  df %<>%
    dplyr::mutate_all(.funs = funs(parse_guess))
  
  # df <- httr::POST(url = "https://sdmdataaccess.sc.egov.usda.gov/tabular/post.rest", 
  #            body = list(query = q, format = "json+columnname") %>%
  #              jsonlite::toJSON(auto_unbox = TRUE)) %>%
  #   httr::stop_for_status() %>%
  #   httr::content(simplifyVector = T) %$%
  #   Table %>%
  #   tibble::as_tibble()
  # 
  # colnames(df) <- df[1,]
  # df <- df[-1,]
  # 
  # df %<>%
  #   dplyr::mutate_all(.funs = funs(parse_guess))

  return(df)
  
}
