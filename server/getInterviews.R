# cases <- get_interviews_for_questionnaire(
#   chunk_size=9900,
#   qnr_id="a686a8b6-b64b-4411-8b37-7bd5174fe720",
#   qnr_version= 2  # This should be an integer, not a string
# )
# 
# 
# cases <- get_interviews_for_questionnaire(
#   chunk_size=210,
#   qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
#   qnr_version= 2  # This should be an integer, not a string
# )
# 

all_users <- get_interviewers()

get_interviews(
  nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
            "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
            "responsibleRole", "supervisorName", "wasCompleted",
           "createdDate", "updateDateUtc",),
  # chunk_size = 100,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

saveRDS(all_users, "server/users.RDS")