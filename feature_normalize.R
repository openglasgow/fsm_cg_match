### Feature normalise for data match

normalize_education = function(education_joined){
  education_norm = education_joined
  hb = education_norm$hb_school_aged_cohort
  roll = education_norm$school_roll
  p1 = education_norm$p1

  ### CRAIGS ADVICE - USE GENDER !!!! ###
  
  ### Add necessary columns
  hb$roll_id = NA
  roll$hb_id = NA
  roll$member_id = NA
  p1$hb_id = NA
  p1$member_id = NA
    
  ### reorder and select the right columns for each
  hb = hb %>% select(claim_id, roll_id, member_id, firstname, middlenames, lastname, dob_y, dob_m, dob_d, lead_firstname, lead_middlenames, lead_lastname, 
                    original_address, flat_position, house_n, house_alpha, street_name, addr_other, post_code )
  roll = roll %>% select(hb_id, seemis_id, member_id, firstname, middlenames, lastname, dob_y, dob_m, dob_d, lead_firstname, lead_middlenames, lead_lastname, 
                         original_address, flat_position, house_n, house_alpha, street_name, addr_other, Pupil.s.postcode)
  p1 = p1 %>% select(hb_id, seemis_id, member_id, firstname, middlenames, lastname, dob_y, dob_m, dob_d, lead_firstname, lead_middlenames, lead_lastname, 
                     original_address, flat_position, house_n, house_alpha, street_name, addr_other, postcode)
  
  ###  Set up normalized colnames
  norm_cols = c("hb_id", "roll_id", "member_id", "dep_firstname", "dep_middlenames", "dep_lastname", "dob_y", "dob_m", "dob_d", "lead_firstname", "lead_middlenames", "lead_lastname",
               "original_address", "flat_position", "house_n", "house_alpha", "street_name", "addr_other", "post_code")
  
  ### Apply cols to the two datasets and append to our list
  colnames(hb) = norm_cols
  colnames(roll) = norm_cols
  colnames(p1) = norm_cols
  education_norm$hb_norm = hb
  education_norm$roll_norm = roll
  education_norm$p1 = p1
  
  return(education_norm)
  
}