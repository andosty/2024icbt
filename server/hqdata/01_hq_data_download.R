sqlSvr <- readRDS("server/credentials/icbt_svr.rds")

#create directory for saving data download if not exist
data_dir <- "Data/"
hqDownload_dir <- paste(data_dir,'HQ_download/',sep='')
hq_extracted_dir <- paste(data_dir,'HQ_extracted/',sep='')

#create Data directory if it does not already exist
ifelse(!dir.exists(file.path(data_dir)),
       dir.create(file.path(data_dir)),
       "Data Directory Exists")

#create HQ_download Director in Data folder if it does not already exist
ifelse(!dir.exists(file.path(hqDownload_dir)),
       dir.create(file.path(hqDownload_dir)),
       "HQ_download Directory Exists")

#create HQ_extracted Director in Data folder if it does not already exist
ifelse(!dir.exists(file.path(hq_extracted_dir)),
       dir.create(file.path(hq_extracted_dir)),
       "HQ_extracted Directory Exists")

# server authentication:
set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace, 
  user =sqlSvr$usr, password = sqlSvr$pswd
)


# Then, filter to the questionnaire of interest and extract the questionnaire ID and version number.
server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == "ICBT MAIN-Field Practice") %>%
  # filter(title == sqlSvr$title) %>%
  # dplyr::pull(questionnaireId)
  # filter(version==max(version))
  filter(version==1)

server_qnr_id <- server_qnr %>%
  # filter(version==6) %>%
  dplyr::pull(id)



# saveRDS(cases,"server/credentials/cases.rds")
# cases <- readRDS("server/credentials/cases.rds")
# 
# options(timeout = max(1000, getOption("timeout")))



# 
# cases <- get_interviews_for_questionnaire(
#   chunk_size=100,
#   # method = curl,
#   qnr_id="a686a8b6-b64b-4411-8b37-7bd5174fe720",
#   qnr_version= 2  # This should be an integer, not a string
# )

# commented out cases bcos of http request error
# cases <- get_interviews_for_questionnaire(
#   chunk_size=100,
#   qnr_id=server_qnr$questionnaireId,
#   qnr_version= server_qnr$version  # This should be an integer, not a string
# )

# 
# cases <- get_interviews_for_questionnaire(
#   chunk_size=100,
#   qnr_id="39ce51e8-dd9a-43ec-95ff-252c40305b18",
#   qnr_version= 1  # This should be an integer, not a string
# )
# print('cases downloaded okay')


download_matching(
  matches = server_qnr$title, 
  export_type = "STATA",
  path = hqDownload_dir,
  # qnr_id= server_qnr_id
)

print('icbt data downloaded okay')

#rename the downloaded file
downloaded_filename <- paste(server_qnr$variable,'_', server_qnr$version,'_STATA_All.zip', sep = '')

# unzip and extract zip file content
zippedFile <- paste(hqDownload_dir, downloaded_filename, sep = '')

if (file.exists(zippedFile)) {
  unzip( zippedFile, exdir=hq_extracted_dir) 
}


# date and time
myDateInfo <-Sys.time()
current_date_raw <- as.character(format(myDateInfo, "%Y-%m-%d %I:%M %p"))   
current_dateOnly <- as.character(format(myDateInfo, "%Y-%m-%d"))   
current_date = gsub(":","-",current_date_raw) 


