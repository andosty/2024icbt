sqlSvr <- readRDS("server/credentials/icbt_svr.rds")
# saveRDS(sqlSvr,"C:/2024ICBT/App/svr/icbt_svr.rds")
# sqlSvr<- sqlSvr %>%
#   mutate(title='ICBT MAIN-PILOT App')


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
  filter(title == sqlSvr$title) %>%
  # dplyr::pull(questionnaireId)
  filter(version==max(version))

server_qnr_id <- server_qnr %>%
  # filter(version==6) %>%
  dplyr::pull(id)
# 
# # start export process; get job ID
# started_job_id <- susoapi::start_export(
#   qnr_id= server_qnr_id,
#   export_type = "STATA",
#   include_meta = TRUE,
#   interview_status = "All",
#   # from = from,
#   # to = to,
#   # credentials
#   server = Sys.getenv("SUSO_SERVER"),     # full server address
#   workspace = Sys.getenv("SUSO_WORKSPACE"),
#   user = Sys.getenv("SUSO_USER"),         # API user name
#   password = Sys.getenv("SUSO_PASSWORD") 
# )
# 
# # CHECK EXPORT JOB PROGESS, UNTIL COMPLETE, specifying ID of job started in prior step
# get_export_job_details(job_id = started_job_id)
# 
# # DOWNLOAD THE EXPORT FILE, ONCE THE JOB IS COMPLETE
# get_export_file(
#   job_id = started_job_id,
#   path = hqDownload_dir,
# )

download_matching(
  matches = server_qnr$title, 
  export_type = "STATA",
  path = hqDownload_dir,
  # qnr_id= server_qnr_id
)

#rename the downloaded file
downloaded_filename <- paste(server_qnr$variable,'_', server_qnr$version,'_STATA_All.zip', sep = '')

# unzip and extract zip file content
zippedFile <- paste(hqDownload_dir, downloaded_filename, sep = '')

if (file.exists(zippedFile)) {
  unzip( zippedFile, exdir=hq_extracted_dir) 
}

# commented out cases bcos of http request error
# cases <- get_interviews_for_questionnaire(
#   chunk_size=100,
#   qnr_id=server_qnr$questionnaireId,
#   qnr_version= server_qnr$version  # This should be an integer, not a string
# )

# date and time
myDateInfo <-Sys.time()
current_date_raw <- as.character(format(myDateInfo, "%Y-%m-%d %I:%M %p"))   
current_dateOnly <- as.character(format(myDateInfo, "%Y-%m-%d"))   
current_date = gsub(":","-",current_date_raw) 


