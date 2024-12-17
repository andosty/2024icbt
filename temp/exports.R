server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == 'ICBT FIELD WORK')  %>%
  # dplyr::pull(questionnaireId)
  filter(version==max(version))

server_qnr_id <- server_qnr %>%
  # filter(version==6) %>%
  dplyr::pull(id)

started_job_id <- susoapi::start_export(
  qnr_id= server_qnr_id,
  export_type = "STATA",
  include_meta = TRUE,
  interview_status = "All",
  # from = from,
  # to = to,
  # credentials
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD")
)


qdoc <- get_questionnaire_document(
  qnr_id=server_qnr_id,
  qnr_version=3,
  path = hqDownload_dir,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

teee <- get_interviews()

# CHECK EXPORT JOB PROGESS, UNTIL COMPLETE, specifying ID of job started in prior step
get_export_job_details(job_id = started_job_id)


#create directory for saving data download if not exist
data_dir <- "../Data/"
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


# DOWNLOAD THE EXPORT FILE, ONCE THE JOB IS COMPLETE
get_export_file(
  job_id = started_job_id,
  path = hqDownload_dir,
)



listAll <- get_export_jobs(
  export_type = "STATA",
  interview_status = "All",
  qnr_id = "49957160-c7e4-4af9-a658-b99111b9104d",
  # export_status = "",
  # has_file = NA,
  # limit = 40,
  # offset = 0,
  # server = Sys.getenv("SUSO_SERVER"),
  # workspace = Sys.getenv("SUSO_WORKSPACE"),
  # user = Sys.getenv("SUSO_USER"),
  # password = Sys.getenv("SUSO_PASSWORD")
)



ssee <- get_export_job_details(
  job_id = 2607,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

get_export_file(
  job_id,
  path,
  verbose = FALSE,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

jj <- get_export_jobs(
  export_type = c("STATA"),
  interview_status = "All",
  qnr_id = "49957160c7e44af9a658b99111b9104d$4",
  export_status = "Completed",
  has_file = NA,
  limit = 40,
  offset = 0,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)


newGET <- get_questionnaires(
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)
