dssat_read_cultigen <- function(cultigen_file) {
  # cat("\n",cultigen_file)
  cultigen_file_all <- readr::read_lines(cultigen_file)
  cultigen_name <- gsub(".CUL","",basename(cultigen_file))

  cultigen_header <- cultigen_file_all[grep("^@",cultigen_file_all)]
  cultigen_header <- gsub("^\\s+|\\s+$", "", cultigen_header)
  cultigen_splits <- as.numeric(gregexpr("[^[:blank:]][[:blank:]]", cultigen_header)[[1]]) + 1
  cultigen_splits_starts <- c(1,7,cultigen_splits[-1])
  cultigen_splits_ends <- c(6,(as.numeric(cultigen_splits)-1)[-1],nchar(cultigen_header))

  cultigen_clean <- cultigen_file_all[!grepl("^!",cultigen_file_all) & !grepl("^@",cultigen_file_all) & !grepl("\\*",cultigen_file_all) & !grepl("^\\$",cultigen_file_all) & !grepl("MINIMA",cultigen_file_all) & !grepl("MAXIMA",cultigen_file_all) & !grepl("DEFAULT",cultigen_file_all)]
  cultigen_clean <- gsub("^\\s+|\\s+$", "", cultigen_clean)
  cultigen_clean <- cultigen_clean[nchar(cultigen_clean) > 0]


  splitter <- function(char_string){
    sapply(1:length(cultigen_splits_starts),function(n){
      out <- paste0(strsplit(char_string,"")[[1]][cultigen_splits_starts[n]:cultigen_splits_ends[n]],collapse='')
      out <- gsub("^\\s+|\\s+$", "", out)
      return(out)
    })
  }

  cultigen_header <- splitter(cultigen_header)
  cultigen_clean <- do.call(rbind,lapply(cultigen_clean,splitter))
  cultigen_clean[cultigen_clean == '.' | cultigen_clean == '-99'] <- NA
  cultigen_clean <- lapply(unlist(apply(cultigen_clean,2,list),recursive = F), type.convert, as.is=T)
  names(cultigen_clean) <- cultigen_header

  cultigen_clean <- tibble::as_tibble(cultigen_clean)

  return(cultigen_clean)
}
