### Export matched datasets

join_matched_ids = function(df){
  id_df = data.frame(hb_id=NA, hb_member=NA, roll_id=NA)
  for(rn in 1:nrow(df)){
    if(rn%%3==0){
      # print(rn)
      pair_1 = df[rn-2,]
      pair_2 = df[rn-1,]
      if((pair_1$Match. == 1 & pair_1$Fran ==1) & (pair_2$Match.==1 & pair_2$Fran==1)){
        id_df = rbind(id_df, data.frame(hb_id=pair_1$hb_id, hb_member=pair_1$member_id, roll_id=pair_2$roll_id))
      }
      # print(pair_1[,"family_id"])
      ### checks to see if 'roll' is in the family_id (DWP it is not)
    }
  }
  return(id_df[2:nrow(id_df),])
}


export_final_dataset = function(final_sets, duplicate_analysis=FALSE){
  ### I MIGHT want to think about using school roll address information in here instead of the parsed dwp information
  ### Also add SEEMIS id
  
  ### data needed: DWP - claimant info, pupil info, roll: seemis id and school info
  
  ### remove non matches from dwp_unmatched
  dwp_final_cohort = final_sets$dwp[final_sets$dwp$dep_id %in% dwp_roll_matched$dwp_dep_id,]
  
  ### Add in seemis id
  dwp_final_cohort = merge(dwp_final_cohort, final_sets$final[,c("dwp_dep_id", "edu_dep_id"),], by.x="dep_id", by.y="dwp_dep_id", all=FALSE)
  
  ### Add in school from roll
  dwp_final_cohort = merge(dwp_final_cohort, data.frame(lapply(final_sets$raw$roll[,c("SEEMIS.reference", "School.Name"),], tolower)), by.x="edu_dep_id", by.y="SEEMIS.reference")
  
  ### Add in claimant title from dwp -- todo - make sure claim_refs with x's are dealt with before match
  dwp_final_cohort = merge(dwp_final_cohort, data.frame(claim_ref = gsub("X", "x", final_sets$raw$dwp$claim_ref), claimant_gender = final_sets$raw$dwp$claimant_gender), by.x="family_id", by.y="claim_ref", all.x=TRUE)
  dwp_final_cohort$title = sapply(trim_all(dwp_final_cohort$claimant_gender), function(x) if(x=="M"){"Mr."}else{"Ms."})
  
  ### Uppercase names
  dwp_final_cohort[,c("claimant_firstname", "claimant_middlenames", "claimant_lastname", "dep_firstname", "dep_middlenames", "dep_lastname")] = lapply(dwp_final_cohort[,c("claimant_firstname", "claimant_middlenames", "claimant_lastname", "dep_firstname", "dep_middlenames", "dep_lastname")], firstup)
  
  ### reorganize dataframe
  dwp_final_cohort = data.frame(seemis_id = dwp_final_cohort$edu_dep_id, dwp_dep_id = dwp_final_cohort$dep_id, dwp_claim_id = dwp_final_cohort$family_id, school_name = dwp_final_cohort$School.Name, dep_firstname = dwp_final_cohort$dep_firstname, dep_middlenames = dwp_final_cohort$dep_middlenames, dep_lastname = dwp_final_cohort$dep_lastname, dep_dob = paste(dwp_final_cohort$dob_y, dwp_final_cohort$dob_m, dwp_final_cohort$dob_d, sep="-"), house_no = dwp_final_cohort$house_no, street_address = dwp_final_cohort$street_address, claimant_title = dwp_final_cohort$title, claimant_firstname = dwp_final_cohort$claimant_firstname, claimant_middlenames = dwp_final_cohort$claimant_middlenames, claimant_lastname = dwp_final_cohort$claimant_lastname, stringsAsFactors = FALSE)
  
  ### check for seemis and dwp duplicates
  if(duplicate_analysis == TRUE){
    
    dup_analysis = merge(dwp_final_cohort, data.frame(lapply(final_sets$raw$roll[,c("SEEMIS.reference", "Forename", "Surname", "Date.of.Birth")], tolower)), by.x="seemis_id", by.y="SEEMIS.reference")
    colnames(dup_analysis)[14:16] = c("roll_firstname", "roll_lastname", "roll_DOB")
    ### Reduce to only show the duplicated rows
    dup_analysis = (dup_analysis[(dup_analysis$seemis_id %in% dup_analysis[duplicated(dup_analysis$seemis_id),]$seemis_id) | (dup_analysis$dwp_dep_id %in% dup_analysis[duplicated(dup_analysis$dwp_dep_id),]$dwp_dep_id),])
    
    ### Save and return
    write.csv(dup_analysis, "./data/export/final_dup_check.csv", row.names=FALSE)
    return()
    
  }
  
  ### Remove duplicates
  dwp_final_cohort = dwp_final_cohort[!duplicated(dwp_final_cohort$dwp_dep_id),]
  dwp_final_cohort = dwp_final_cohort[!duplicated(dwp_final_cohort$seemis_id),]
  
  ### Remove applications
  dwp_final_cohort = dwp_final_cohort[!(dwp_final_cohort$seemis_id %in% final_sets$applications$dep_id.y),]
  
  #   ### create aggregate columns
  #   dwp_final[,c("claimant_name", "dep_name", "dep_dob")] = c(trim_all(paste(dwp_final$claimant_firstname, dwp_final$claimant_middlenames, dwp_final$claimant_lastname)),  trim_all(paste(dwp_final$dep_firstname, dwp_final$dep_middlenames, dwp_final$dep_lastname)), trim_all(paste(dwp_final$dob_y, dwp_final$dob_m, dwp_final$dob_d, sep="-")))  
  #   
  #   ### Don't I need to cast this? - slice for now
  #   dwp_cast_name = cast_long_df(dwp_final[1:127,], "family_id", "dep_name", "dep_name_")
  #   dwp_cast_dob = cast_long_df(dwp_final[1:127,], "family_id", "dep_dob", "dep_dob_")
  #   
  #   ### merge the two
  #   dwp_cast_merged = merge(dwp_cast_name, dwp_cast_dob[,c("family_id", "dep_dob_1", "dep_dob_2", "dep_dob_3", "dep_dob_4")], by="family_id", all.x=TRUE)
  #   
  #   ### Subset to get the final article
  #   dwp_final= subset(dwp_cast_merged, select=c(family_id, dep_id, seemis_id, claimant_name, house_no, street_address, postcode, dep_name_1, dep_dob_1, dep_name_2, dep_dob_2, dep_name_3, dep_dob_3, dep_name_4, dep_dob_4))
  #   
  ### Write out
  write.csv(dwp_final_cohort, paste("./data/live/final_export/dwp_final_cohort_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
  write.csv(final_sets$applications[is.na(final_sets$applications$dep_id.y),], paste("./data/live/final_export/applications_no_string_match_", Sys.Date(), ".csv", sep=""), row.names=FALSE)
}