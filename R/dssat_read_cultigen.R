dssat_read_cultigen <- function(cultigen_file) {
  temp <- tempfile()
  on.exit(unlink(temp))
  
  # cat("\n",cultigen_file)
  readr::read_lines(cultigen_file) %>%
    gsub("!.*", "", .) %>%
    trimws("right") %>%
    magrittr::extract(. != "") %>%
    magrittr::extract(-1:-2) %>%
    readr::write_lines(temp)
  
  readr::read_fwf(temp,
                  col_positions = readr::fwf_positions(start = c(1, 8, 25, 31, 38, 44, 50, 56, 62, 68),
                                                       end =   c(6, 23, 29, 36, 42, 48, 54, 60, 66, 72),
                                                       col_names = c("@VAR#",
                                                                     "VRNAME..........",
                                                                     "EXPNO",
                                                                     "ECO#",
                                                                     "P1",
                                                                     "P2",
                                                                     "P5",
                                                                     "G2",
                                                                     "G3",
                                                                     "PHINT")),
                  col_types = readr::cols(
                    `@VAR#` = readr::col_character(),
                    `VRNAME..........` = readr::col_character(),
                    `EXPNO` = readr::col_character(),
                    `ECO#` = readr::col_character(),
                    P1 = readr::col_double(),
                    P2 = readr::col_double(),
                    P5 = readr::col_double(),
                    G2 = readr::col_double(),
                    G3 = readr::col_double(),
                    PHINT = readr::col_double()
                  ))
  
}
