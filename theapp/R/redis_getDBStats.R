# need to replace that first line in the function `dbGetInfo()` with `redisInfo()` and the rest should kinda work.
getDBStats = function() {
  dbstats<- dbGetInfo()

  out_tab<-  tryCatch(expr = {
    last_saved<- as.POSIXct(as.numeric(dbstats[['rdb_last_save_time']]), origin = '1970-01-01 00:00:00')

    tab<- data.frame(Category = "DB", Feature = c('Redis Version', "Operating System", "Arch", "Uptime in days", "Keyspace",
                                                  "Last Save Status", "Changes since last save", 'Last save date'),
                     Values = c(dbstats[['redis_version']], dbstats[['os']], dbstats[['arch_bits']],
                                dbstats[['uptime_in_days']], dbstats[['db0']], dbstats[['rdb_last_bgsave_status']],
                                dbstats[['rdb_changes_since_last_save']], strftime(last_saved,format = '%Y-%d-%m %H:%M:%S')))

    gbfactor<- (1024*1024*1024)#Kilobytes, megabytes, gigabytes
    roundingDigits<- 2

    used<- as.numeric(dbstats[['used_memory']])/ gbfactor
    avail<- as.numeric(dbstats[['total_system_memory']])/gbfactor #gigabytes available.

    tab2<- data.frame(Category = 'Memory', Feature = c("Available memory", "Used memory", "Percentage Used"),
                      Values = c(paste0(round(avail,roundingDigits), " GB"),
                                 paste0(round(used,roundingDigits), " GB"),
                                 paste0(round((used/avail * 100),roundingDigits), "%")))

    net_total_in<- as.numeric(dbstats[['total_net_input_bytes']]) / gbfactor
    net_total_out<- as.numeric(dbstats[['total_net_output_bytes']]) /gbfactor

    tab3 <- data.frame(Category = 'Performance', Feature= c("Net data in", "Net data out", "Commands processed"),
                       Values = c(paste0(round(net_total_in, roundingDigits), " GB"),
                                  paste0(round(net_total_out, roundingDigits), " GB"),
                                  dbstats[['total_commands_processed']]))
    return(rbind(tab,tab2,tab3))
  }, error = function(e) { return(data.frame(Status = 'Unable to retrieve DB status')) })

  return(out_tab)
}
