read_cultigen <- function(cultigen.file) {
  # cat("\n",cultigen.file)
  cultigen.file.all <- read_lines(cultigen.file)
  cultigen.name <- gsub(".CUL","",basename(cultigen.file))

  cultigen.header <- cultigen.file.all[grep("^@",cultigen.file.all)]
  cultigen.header <- gsub("^\\s+|\\s+$", "", cultigen.header)
  cultigen.splits <- as.numeric(gregexpr("[^[:blank:]][[:blank:]]", cultigen.header)[[1]]) + 1
  cultigen.splits.starts <- c(1,7,cultigen.splits[-1])
  cultigen.splits.ends <- c(6,(as.numeric(cultigen.splits)-1)[-1],nchar(cultigen.header))

  cultigen.clean <- cultigen.file.all[!grepl("^!",cultigen.file.all) & !grepl("^@",cultigen.file.all) & !grepl("\\*",cultigen.file.all) & !grepl("^\\$",cultigen.file.all) & !grepl("MINIMA",cultigen.file.all) & !grepl("MAXIMA",cultigen.file.all) & !grepl("DEFAULT",cultigen.file.all)]
  cultigen.clean <- gsub("^\\s+|\\s+$", "", cultigen.clean)
  cultigen.clean <- cultigen.clean[nchar(cultigen.clean) > 0]


  splitter <- function(char.string){
    sapply(1:length(cultigen.splits.starts),function(n){
      out <- paste0(strsplit(char.string,"")[[1]][cultigen.splits.starts[n]:cultigen.splits.ends[n]],collapse='')
      out <- gsub("^\\s+|\\s+$", "", out)
      return(out)
    })
  }

  cultigen.header <- splitter(cultigen.header)
  cultigen.clean <- do.call(rbind,lapply(cultigen.clean,splitter))
  cultigen.clean[cultigen.clean == '.' | cultigen.clean == '-99'] <- NA
  cultigen.clean <- lapply(unlist(apply(cultigen.clean,2,list),recursive = F), type.convert, as.is=T)
  names(cultigen.clean) <- cultigen.header

  cultigen.clean <- tbl_df(cultigen.clean)

  return(cultigen.clean)
}
