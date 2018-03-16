EmployeeName_to_FirstName <- function(EmployeeName){
  FirstName <- gsub(pattern = " .*", replacement = "", x = EmployeeName)
  return(FirstName)
}
