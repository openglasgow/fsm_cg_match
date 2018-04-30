### process LLR files
process_education = function(education_raw){
  ### clone education
  education_processed = education_raw
  
  ### Trim and tolower everything
  education_processed = lapply(education_processed, function(x) import_string_normalize(x))
  cat("*** All Datasets string normalized \n")
  
  ### Individual processing
  cat("*** Members Processing \n")
  members_processed = process_members(education_processed$members)
  cat("*** Incomes Processing \n")
  incomes_processed = process_incomes(education_processed$incomes)
  cat("*** Households Processing \n")
  households_processed = process_households(education_processed$households)
  cat("** School Roll Processing \n")
  school_roll_processed = process_school_roll(education_processed$school_roll)
  cat("** P1 processing\n")
  p1_processed = process_p1(education_processed$p1)
  cat("** last years processing\n")
  match_2016_processed = process_match_2016(education_processed$match_2016)
  ### Return list of processed datasets
  return(list(households= households_processed, incomes=incomes_processed, members=members_processed, school_roll = school_roll_processed, match_2016 = match_2016_processed, p1 = p1_processed))
}

process_members = function(members_raw){
  members_processed = members_raw
  
  member_colnames = c("claim_id","house_id","member_id","name","surname","title","forename","find_name","type","birth_date","gender","carer_ind","disable_ind","hospital_ind","meals_ind","shared_perc","student_ind","hours_worked","cared_for_ind","school_ind","nino","self_empl_ind","incapable_of_work","weeks_28","weeks_52","residential_acc","mat_leave_ccp_ind","holocaust_disregard","srplumpsum_disregard","ccp_prison_hosp_ind","ccp_rem_work_ind","advantageous_change_ind","non_dep_gc_sc_ind","non_dep_easement_ind","easement_date","disregard_50","non_dep_grp","severely_mentally_impaired_ind")
  colnames(members_processed) = member_colnames
  
  ### convert index to category
  member_type_keys = read.csv("../data/import/keys/member_type.csv", strip.white = TRUE, stringsAsFactors = FALSE)
  members_processed = members_processed %>% mutate(type = 
                                                   unlist(left_join(members_processed, 
                                                                     member_type_keys, 
                                                                     by=c("type" = "code")) %>% 
                                                          select(type.y)))
  
  ### Normalise names
  members_processed[,c("firstname", "middlenames", "lastname")] = top_tail_names(apply(members_processed[,c("forename", "surname")], 1, paste, collapse=" "))
  
  ### To date
  members_processed = members_processed %>% mutate(birth_date = as.Date(birth_date, format="%Y/%m/%d"))
  
  ### Normalise DOB
  members_processed[,c("dob_y", "dob_m", "dob_d")] = members_processed %>% mutate(dob_y = format(birth_date, "%Y"), dob_m = format(birth_date, "%m"), dob_d=format(birth_date, "%d")) %>% select(dob_y, dob_m, dob_d)
  
  return(members_processed)
}

process_incomes = function(incomes_raw){
  incomes_processed = incomes_raw
  income_colnames = c("claim_id","house_id","member_id","inc_amt","ni_amt","tax_amt","pension_amt","freq_period","freq_len","inc_code","award_date","end_date","highest_deduct_ind","wtx_ctx_amt","lowest_deduct_ind","non_dep_easement_ind","disreg_type")
  colnames(incomes_processed) = income_colnames
  
  ### convert index to category for member_types

  ### to date
  incomes_processed = incomes_processed %>% mutate(award_date = as.Date(award_date, format("%Y/%m/%d")))
  
  ### process weekly income
  incomes_processed = add_calculated_weekly(incomes_processed)
  
  return(incomes_processed)
}

process_households = function(households_raw){
  households_processed = households_raw
  household_colnames = c("claim_id","house_id","last_upd","from_date","to_date","inc_supp_ind","claim_type_ind","addr1","addr2","addr3","addr4","post_code","srr_exempt","lp_protect_ind","cbl_protect_ind","shared_accom_ind","group_id","ben_cap_level_ind","uc_dhp_ind","ctax_arrears", "tenure_type")
  colnames(households_processed) = household_colnames
  
  ### Convert index to category
  
  ### to date
  households_processed[,c("from_date", "to_date")] = lapply(households_processed[,c("from_date", "to_date")], function(x) as.Date(x, format="%Y/%m/%d"))
  
  ### parse addresses
  addr_vec = gsub("\\s?NA\\s?", "", apply(households_processed[, c("addr1", "addr2", "addr3", "addr4")], 1, paste, collapse=" "))
  households_processed = cbind(households_processed, address_norm(addr_vec))
  
  return(households_processed)
}

process_school_roll = function(school_roll_raw){
  school_roll_processed = school_roll_raw
  
  ### Normalize names
  school_roll_processed[,c("firstname", "middlenames", "lastname")] = top_tail_names(apply(school_roll_processed[,c("Forename", "Surname")], 1, paste, collapse=" "))
  school_roll_processed[,c("lead_firstname", "lead_middlenames", "lead_lastname")] = top_tail_names(apply(school_roll_processed[,c("Contact.s.forename", "Contact.s.surname")], 1, paste, collapse=" "))
  
  ### date and split bdays
  school_roll_processed = school_roll_processed %>% mutate(Date.of.Birth = as.Date(Date.of.Birth, "%d-%b-%y"))
  school_roll_processed[,c("dob_y", "dob_m", "dob_d")] = school_roll_processed %>% mutate(dob_y = format(Date.of.Birth, "%Y"), dob_m = format(Date.of.Birth, "%m"), dob_d=format(Date.of.Birth, "%d")) %>% select(dob_y, dob_m, dob_d)
  
  ### normalize addresses
  addr_vec = gsub("\\s?NA\\s?", "", apply(school_roll_processed[, c("Pupil.s.property", "Pupil.s.street")], 1, paste, collapse=" "))
  school_roll_processed = cbind(school_roll_processed, address_norm(addr_vec))

  ### Rename seemis ref
  school_roll_processed = school_roll_processed %>% rename(seemis_id = SEEMIS.reference)
  
  ### subset and rename columns
  school_roll_processed = school_roll_processed %>% select(seemis_id, firstname, middlenames, lastname, lead_firstname, lead_middlenames, lead_lastname, 
                                                           dob_y, dob_m, dob_d, original_address, flat_position, house_n, house_alpha, street_name, 
                                                           addr_other, Pupil.s.postcode)
  
  return(school_roll_processed)
  
}

process_p1 = function(p1_raw) {
  p1_processed = p1_raw
  
  ### Normalize names
  p1_processed[,c("firstname", "middlenames", "lastname")] = top_tail_names(apply(p1_processed[,c("Forename", "Surname")], 1, paste, collapse=" "))
  p1_processed[,c("lead_firstname", "lead_middlenames", "lead_lastname")] = NA
  
  ### date and split bdays
  p1_processed = p1_processed %>% mutate(DOB = as.Date(DOB, "%Y-%m-%d"))
  p1_processed[,c("dob_y", "dob_m", "dob_d")] = p1_processed %>% mutate(dob_y = format(DOB, "%Y"), dob_m = format(DOB, "%m"), dob_d=format(DOB, "%d")) %>% select(dob_y, dob_m, dob_d)
  
  ### normalize addresses
  addr_vec = str_match(p1_processed$Address, "(.+?)(, .*)$")[,2]
  p1_processed = cbind(p1_processed, address_norm(addr_vec))
  p1_processed$postcode = as.vector(str_match(p1_processed$Address, "\\w+ \\w+$"))
  
  ### Seemis rename
  p1_processed = p1_processed %>% rename(seemis_id = SEEMIS.ID)
  
  ### Select
  p1_processed = p1_processed %>% select(seemis_id, firstname, middlenames, lastname, lead_firstname, lead_middlenames, lead_lastname, 
                                                           dob_y, dob_m, dob_d, original_address, flat_position, house_n, house_alpha, street_name, 
                                                           addr_other, postcode)
  
  return(p1_processed)
}

process_match_2016 = function(match_2016_raw){
  match_2016_processed = match_2016_raw
  
  return(match_2016_processed)
}
### Try to abstract turning the inc_amt from incomes into a weeky amount
add_calculated_weekly = function(incomes){
  
  incomes_formula = function(incomes_row){
    # incomes_row = data.frame(t(incomes_row), stringsAsFactors=FALSE)
    if(incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 0){
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_len"] * 7
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 0) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 7
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 2) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"]
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 2) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"])
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 3) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 3) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 12 / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 4) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 2 / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 4) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) * 2 / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 5) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 5) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / 52
    } else if (incomes_row["freq_period"] != 0 & incomes_row["freq_len"] == 14) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / incomes_row["freq_period"] / 52
    } else if (incomes_row["freq_period"] == 0 & incomes_row["freq_len"] == 14) {
      (incomes_row["inc_amt"] - incomes_row["ni_amt"] - incomes_row["tax_amt"]) / 52
    } else {
      -1
    }
  }
  
  ### Apply the formula row wise
  weekly_income = apply(incomes[,c("inc_amt", "ni_amt", "tax_amt", "freq_len", "freq_period")], 1, incomes_formula)
  
  ### add it to our DF
  incomes$weekly_income_calculated = weekly_income
  
  return(incomes)
  
}

