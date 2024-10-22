##cases to reject
CaseReject<- errorChecks %>%
  group_by(interview_key,responsibleId,interview_id) %>%
  distinct(errorCheck) %>%
  summarize(errorMessage = str_c(errorCheck, collapse = " ; ")) %>%
  ungroup() 

##send cases back
for (i in 1:nrow(CaseReject)  ) {
  row <- CaseReject[i,]  #filter that row
  print(row$interview_key)
  #send the case back as rejected
  #Rejecting a case as hq
  rejections<- reject_interview_as_hq(
    interview_id=row$interview_id,
    comment = row$errorMessage,
    responsible_id = row$responsibleId,
    verbose = FALSE
  )

}
