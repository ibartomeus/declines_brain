#Upload files to google drive
#install packages
#remotes::install_github("claudiozandonella/trackdown",
#                        build_vignettes = TRUE, force=T)

#install.packages("googledrive")



library(googledrive)
library(trackdown)

#set google doc
googledrive::drive_auth()

#upload file

#update_file
update_file(
  file = "Manuscript/Ms.Rmd", 
  gfile = "Bee_brains"
)


#download file 
download_file(
  file = "Manuscript/Ms.Rmd", 
  gfile = "Bee_brains"
)
