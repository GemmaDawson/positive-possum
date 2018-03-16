# # -------------------------------------------------------------------------
# # Copyright (C) 2016-2018 Pivot Sciences Pty Ltd - All Rights Reserved
# # Unauthorized copying of this file, via any medium is strictly prohibited
# #
# # NOTICE: The content herein is proprietary and confidential and remains
# # the property of Pivot Sciences Pty Ltd and its suppliers, if any.  
# # The intellectual and technical concepts contained herein are proprietary.
# # Dissemination of this information or reproduction of this material
# # is strictly forbidden unless prior written permission is obtained
# # from Pivot Science Pty Ltd.
# # For more information please contact <info@pivotsciences.com>
# # -------------------------------------------------------------------------
# 
# 
# #error handling example
# logger<-function(message, e = '', showdetail = F, detailmessage=NULL, echo = F, username=NULL){
#   #this does assume we can actually write to file. If the server runs out of space or we have a
#   #permission issue then this will fail.
#   f<- file(paste0(codefolder, "logs/", "log.",as.character(str_replace(Sys.time(), pattern = " ", replacement = ".")),".txt"), open="a")
#   cat(paste0(Sys.time()," | ", username,  " | ", message, ": ", e, "\n"), file = f)
#   close(f)
#   
#   if(echo){
#     cat(paste0(message, ": ", e,"\n")) # so that it displays on the console if you're monitoring the server. always display the detail
#     #for shinyBS
#     #createAlert(session, "alert", title = "Whoops",  content = message, dismiss = TRUE, append = TRUE)
#   }
#   if(showdetail){ 
#     message <- ifelse(is.null(detailmessage), paste0(message,": \n", e), detailmessage)
#     sendSweetAlert(messageId = "warningSw", title = "Whoops... something went wrong.", text = message, type = "error")
#   }
# }