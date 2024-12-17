getDet <- downloaded_icbt_data %>%
  filter(
    ! (
        (str_to_upper(str_squish(trim(enumerator_name)))=="OPPONG SIMON" & interview_key=="48-21-24-13" & interview_id== "5eae3f4bf3c64527b311375d0e857182" )  | 
        (str_to_upper(str_squish(trim(enumerator_name)))=="OPPONG SIMON" & interview_key=="52-21-39-28" & interview_id== "3dd71d59fd2c43169701b5550b4e6bba" )  | 
       (str_to_upper(str_squish(trim(enumerator_name)))=="AGYEIWAA ADUSAH MARY" & interview_key=="17-68-34-17" & interview_id== "b8f22ddaa5614cdc9d3cbc7fa0f3e39e" ) 
      )
    )