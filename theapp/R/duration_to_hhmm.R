duration_to_hhmm <- function(duration){
  hhmm <- stringr::str_c(sprintf("%02d", duration - (duration %% 1)),
                sprintf("%02d", round((duration %% 1)*60)),
                sep = ":")
  return(hhmm)
}
