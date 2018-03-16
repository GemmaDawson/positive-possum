test_and_write_redis <- function(input.df_ = values$df.date, EntryDate_ = input$date, user.dataframe, conf_ = config$rediscontainer, append = FALSE){
  toWrite = morph_to_redis_format_from_timecapture(df.input = input.df_, captureDate = Sys.Date(), userDataframe = user.dataframe)
  # if nothing went wrong here then push to redis
  if(is.data.frame(toWrite)){
    shortName_ = user.dataframe$shortName
    TimeEntryDate_ = EntryDate_
    keyToWrite_ = paste(shortName_, as.character(TimeEntryDate_), "timecapture", sep = ":")
    # keyToWrite_ = create_timecapture_key(shortName = shortName_, TimeEntryDate = TimeEntryDate_)
    # redisSet(key = keyToWrite_, value = toWrite)
    if(append == TRUE){
      toWrite = rbind(toWrite, redisGetThis(conf = conf_, key = keyToWrite_))
    }
    redisSetThis(conf = conf_, key = keyToWrite_, value = toWrite)
  }
}
