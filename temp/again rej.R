allErrors <- readRDS("")

thisUser<- all_users %>% filter(UserName=="f16056")


anotherUser <- all_users %>% filter(str_detect(str_to_upper(FullName),"ANYAMASA MARTINA"))



