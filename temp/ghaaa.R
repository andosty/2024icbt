#rad icbt error
#rad icbt data

infor <- icbt_data %>%
  filter(str_to_title(enumerator_name)=="Patrick Mensah Dzidzornu")  %>%
  distinct(responsibleId, interview_id, enumerator_name, interview_key )




 stats <- get_interview_stats(
    '9fc79ff93b374b15b6a1826a12d75cf4'
  )

  log <- get_user_action_log(
    user_id = stats$ResponsibleId,
    start =  as.character(today()-1),
    end = as.character(today()+1)
  ) %>% filter( Message !="Sync started (Online)")

  
  reject_interview_as_hq(
    interview_id= stats$InterviewId,
    comment = "check and correct your errors",
    responsible_id = stats$ResponsibleId,
    verbose = TRUE
  )