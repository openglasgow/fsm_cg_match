### Joining up attributes from datasets
join_education = function(education_processed){
  education_joined = education_processed
  
  ### subset children who are school aged
  start_cutoff = as.Date("2013-02-28")
  end_cutoff = as.Date("1999-03-01")
  members_school_aged = education_joined$members %>% filter(birth_date <= start_cutoff & birth_date >= end_cutoff) 
  
  ### pull across household information
  members_school_aged = left_join(members_school_aged %>% 
              select(claim_id, member_id, firstname, middlenames, lastname, type, birth_date, dob_y, dob_m, dob_d, gender), 
            (education_joined$households %>% 
               select(claim_id, flat_position, original_address, house_n, house_alpha, street_name, addr_other, post_code)), 
            by=c("claim_id"))
  
  ### pull across incomes information (aggregate)
  members_school_aged = left_join(members_school_aged, 
            (education_joined$incomes %>% 
               group_by(claim_id) %>% summarize(claim_weekly_household_income = sum(weekly_income_calculated))), 
            by=c("claim_id")) %>% 
    left_join(., 
              (education_processed$incomes %>% 
                 group_by(claim_id, member_id) %>% summarize(claim_weekly_personal_income = sum(weekly_income_calculated))),
              by=c("claim_id"="claim_id", "member_id"="member_id"))
  
  ## Get lead household member details across
  members_school_aged = left_join(members_school_aged, 
                                  (education_joined$members %>% filter(member_id ==1) %>% 
                                     select(claim_id, firstname, middlenames, lastname) %>% 
                                     rename(lead_firstname=firstname, lead_middlenames = middlenames, lead_lastname = lastname)),
                                  by=c("claim_id"))
  
  ## Rename, because reasons.
  education_joined$hb_school_aged_cohort = members_school_aged
  
  return(education_joined)
}