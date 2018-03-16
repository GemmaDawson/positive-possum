update_pivot_members_df <- function(user.data_, user.data.old_){
  goodToGo <- assertthat::assert_that(all(c("EmployeeName", "EmailAddress", "shortName", "PreferredName", "Current", "Admin") %in% names(user.data_))) ||
    assertthat::assert_that(all(c("EmployeeID", "EmployeeName", "EmailAddress", "PreferredName", "Current", "Admin") %in% names(user.data.old_)))
  tryCatch({
    if(goodToGo){
      user.data_$shortName <- NULL
      user.data.old_$EmployeeID <- NULL
      user.data_ <- rbind(user.data_, user.data.old_[!(user.data.old_$EmployeeName %in% user.data_$EmployeeName),])
      return(user.data_)
    } else {
      stop()
    }
  }
  , error = function(e){paste0(e)}
  )
}