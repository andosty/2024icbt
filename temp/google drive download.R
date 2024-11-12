# setwd("C:/2024GCA++/minorSeason/scripts/googledocs")

# download from google#load the libraries
library(stringr)
if(!require(googledrive)) install.packages("googledrive")
library(googledrive)

#for Main Season
folder_id <-  drive_get(as_id("1NaOpIcj-O3OxXDRTMVAmZED6rUP2rhHH"))
#find files in folder
files = drive_ls(folder_id)

#loop dirs and download files inside them
for (i in seq_along(files$name)) {
  #get dataframe-list files
  public_file <-  drive_get(as_id(files[i, ]$id))

  #download files
  for (file_i in seq_along(public_file$name)) {
    #fails if already exists
    try({
      drive_download(
        as_id(public_file$id[file_i]),
        path = str_c("C:/2024GCA++/minorSeason/scripts/googledocs/phase_1_yieldStudies_mainSeason/", public_file$name[file_i]),
        overwrite = FALSE
        # path = str_c(files$name[i])
      )
    })
  }
}
