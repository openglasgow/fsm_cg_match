### Run ###
source("init.R")
#education_raw = import_education()
education_joined = import_education() %>% process_education() %>% join_education()

education_norm = normalize_education(education_joined)

### Join hb and roll determinalistically
### conditions -> dep_firstname, dep_lastname, dob_y, dob_m, dob_d all the same
hb_roll_exact_matches = inner_join(education_norm$hb_norm, education_norm$roll_norm, by=c("dep_firstname", "dep_lastname", "dob_y", "dob_m", "dob_d"))

### Remove exact_matches from datasets
hb_unmatched = education_norm$hb_norm %>% filter(!(paste(hb_id, member_id) %in% paste(hb_roll_exact_matches$hb_id.x, hb_roll_exact_matches$member_id.x)))
roll_unmatched = education_norm$roll_norm %>% filter(!(roll_id %in% hb_roll_exact_matches$roll_id.y))

### Record Linkage
exclude_cols = c("hb_id", "roll_id", "member_id", "dep_middlenames", "lead_middlenames", "house_alpha", "addr_other", "original_address")
hb_roll_pairs = generate_pairs(hb_unmatched, roll_unmatched, exclude_cols)

### Save the file for recovery later
save_rl_file(hb_roll_pairs$pairs, paste("../data/rl_data/", Sys.Date, ".zip", sep=""))

### investigate the frequency of match scores
plot(hb_roll_pairs$hist$counts[30:60])

### Set the thresholds
t_lower = .61
t_upper = .70

### Classify the pairs
hb_roll_classified = classify_pairs(hb_roll_pairs$pairs, lower_threshold=t_lower, upper_threshold=t_upper)

### Get these badboys to disk
write.csv(hb_roll_classified$review, paste("../data/output/hb_roll_classified_", Sys.Date(), ".csv", sep=""), row.names=FALSE)

### *** MANUAL CHECK EXERCISE *** ### 

### import matched dataset
hb_roll_matched = read.csv("../data/import/hb_roll_matches_2017_06_16.csv", stringsAsFactors = FALSE)

### matched ids
matched_ids = join_matched_ids(hb_roll_matched)
matched_ids = data.frame(lapply(matched_ids, function(x) as.character(x)), stringsAsFactors = FALSE)

### join education / hb
colnames(education_norm$roll_norm) = paste("ROLL_", colnames(education_norm$roll_norm), sep="")
colnames(education_norm$hb_norm) = paste("HB_", colnames(education_norm$hb_norm), sep="")
matched_final = left_join(matched_ids, education_norm$roll_norm, by=c("roll_id" = "ROLL_roll_id"))
matched_final = left_join(matched_final %>% mutate(hb_id_member = paste(hb_id, hb_member)), education_norm$hb_norm %>% mutate(hb_id_member = paste(HB_hb_id, HB_member_id)), by=c("hb_id_member"))

### Sort exact matches into format
exact_ids = hb_roll_exact_matches %>% 
            select(hb_id.x, member_id.x, roll_id.y) %>% 
            rename(hb_id = hb_id.x, hb_member = member_id.x, roll_id = roll_id.y) %>% 
            mutate(hb_id_member = paste(hb_id, hb_member))
                                      
exact_matches = left_join(exact_ids, education_norm$hb_norm %>% mutate(hb_id_member = paste(HB_hb_id, HB_member_id)), by=c("hb_id_member")) %>% 
  left_join(., education_norm$roll_norm, by=c("roll_id" = "ROLL_roll_id"))

### remove duplicates and NA cols
matched_final = matched_final %>% filter(!duplicated(hb_id_member), !duplicated(roll_id)) %>% select(-c(HB_roll_id, ROLL_hb_id, ROLL_member_id))
matched_final = rbind(matched_final, exact_matches[,colnames(matched_final)])

### Add FSM/EMA information 
incomes_household = education_norm$incomes[] %>% 
  group_by(claim_id) %>% 
  summarize(income_codes = paste(inc_code, collapse=","), yearly_household_income=sum(weekly_income_calculated)*52) %>% 
  mutate(WTX_elligible = as.vector(!is.na(str_match(income_codes, "wtx"))),
         JSC_elligible = as.vector(!is.na(str_match(income_codes, "jsc"))),
         JSA_elligible = as.vector(!is.na(str_match(income_codes, "jsi"))),
         JSL_elligible = as.vector(!is.na(str_match(income_codes, "jsl"))),
         CTX_eligible = as.vector(!is.na(str_match(income_codes, "ctx"))),
         IS_eligible = as.vector(!is.na(str_match(income_codes, "ins"))),
         ESA_eligible = as.vector(!is.na(str_match(income_codes, "esa"))),
         UCA_eligible = as.vector(!is.na(str_match(income_codes, "uca"))),
         earned_income = as.vector(!is.na(str_match(income_codes, "ear"))),
         claim_id = as.character(claim_id))

matched_final = left_join(matched_final, incomes_household, by=c("hb_id" = "claim_id")) %>%
  filter(!duplicated(roll_id)) %>% 
  mutate(hb_dob = as.Date(paste(HB_dob_y, HB_dob_m, HB_dob_d, sep="-"))) %>% 
  mutate(under_16 = hb_dob <= Sys.Date()-(16*365),
         income_under_6420 = yearly_household_income <= 6420,
         income_under_26_5k = yearly_household_income <= 26500,
         born_between_1mar1997_28feb_2001 = (hb_dob >= as.Date('1997-03-1') & hb_dob <= as.Date('2001-02-28'))) %>%
  left_join(., (education_raw$school_roll %>% select(SEEMIS.reference, School.Name) %>% mutate(SEEMIS.reference = trim_all(tolower(SEEMIS.reference)))), by=c("roll_id" = "SEEMIS.reference"))

### Write out file 
write.csv(matched_final[1:(nrow(matched_final)/2),], paste("../data/export/matched_final_less_p1_", Sys.Date(), "_part_1", ".csv", sep=""), row.names=FALSE)
write.csv(matched_final[((nrow(matched_final)/2)+1):nrow(matched_final),], paste("../data/export/matched_final_less_p1_", Sys.Date(), "_part_2", ".csv", sep=""), row.names=FALSE)

### Take P1 and remove last years SEEMIS ID's out, and do a data match against this years roll less the ones we already have from above
education_norm$p1$post_code = as.vector(education_norm$p1$post_code)
p1_reduced = education_norm$p1 %>% filter(!(roll_id %in% education_norm$match_2016$seemis_id)) 
hb_reduced = education_norm$hb_norm %>% mutate(hb_id_member = paste(hb_id, member_id))  %>% filter(!(hb_id_member %in% matched_final$hb_id_member)) %>% select(-hb_id_member)

### Record Linkage
exclude_cols = c("hb_id", "roll_id", "member_id", "dep_middlenames", "lead_middlenames", "house_alpha", "addr_other", "original_address", "lead_firstname", "lead_middlenames", "lead_lastname")
p1_hb_pairs = generate_pairs(p1_reduced, hb_reduced, exclude_cols)

### Save the file for recovery later
save_rl_file(p1_roll_reduced_pairs$pairs, paste("../data/rl_data/", Sys.Date, ".zip", sep=""))

### investigate the frequency of match scores
plot(p1_roll_pairs$hist$count[68:100])

### Set the thresholds
t_lower = .68
t_upper = .75

### Classify the pairs
p1_hb_reduced_classified = classify_pairs(p1_hb_pairs$pairs, lower_threshold=t_lower, upper_threshold=t_upper)

### Get these badboys to disk
write.csv(p1_hb_reduced_classified$review, paste("../data/export/p1_hb_reduced_classified_", Sys.Date(), ".csv", sep=""), row.names=FALSE)

# *** Last years analysis ***

### More analysis on last years clothing grant
### Import CG and prepare it for processing
cg_paid = read.csv("../data/import/cg_paid_16-17.csv", stringsAsFactors = FALSE)
cg_paid = import_string_normalize(cg_paid)
cg_paid = cg_paid %>% mutate(Date.of.Birth = as.Date(Date.of.Birth, format("%d/%b/%Y")))

cg_paid = cg_paid %>% mutate(name_dob = paste(Pupil.Forename, Pupil.Surname, Date.of.Birth))
matched_final_cg_match = matched_final %>% mutate(name_dob = trim_all(paste(ROLL_dep_firstname, ROLL_dep_middlenames, ROLL_dep_lastname, paste(ROLL_dob_y, ROLL_dob_m, ROLL_dob_d, sep="-"))))

cg_paid_vs_matched_match_left = left_join(matched_final_cg_match, cg_paid, by="name_dob") %>% filter(!duplicated(name_dob))
cg_paid_vs_matched_cg_left = left_join(cg_paid, matched_final_cg_match, by="name_dob") %>% filter(!duplicated(name_dob))

### Write out the people who are currently on
write.csv(cg_paid_vs_matched_match_left[1:(nrow(cg_paid_vs_matched_match_left)/2),], paste("../data/export/cg_paid_vs_matched_cr_code_", Sys.Date(), "_part_1", ".csv", sep=""), row.names=FALSE)
write.csv(cg_paid_vs_matched_match_left[((nrow(cg_paid_vs_matched_match_left)/2)+1):nrow(cg_paid_vs_matched_match_left),], paste("../data/export/cg_paid_vs_matched_cr_code_", Sys.Date(), "_part_2", ".csv", sep=""), row.names=FALSE)


### Write out the people who dropped off
write.csv(cg_paid_vs_matched_cg_left[1:(nrow(cg_paid_vs_matched_cg_left)/2),], paste("../data/export/cg_paid_vs_matched_", Sys.Date(), "_part_1", ".csv", sep=""), row.names=FALSE)
write.csv(cg_paid_vs_matched_cg_left[((nrow(cg_paid_vs_matched_cg_left)/2)+1):nrow(cg_paid_vs_matched_cg_left),], paste("../data/export/cg_paid_vs_matched_", Sys.Date(), "_part_2", ".csv", sep=""), row.names=FALSE)
# write.csv(cg_paid_vs_matched_cg_left, paste("../data/export/cg_paid_vs_matched_", Sys.Date(), ".csv", sep=""), row.names=FALSE)

### Start with Cg paid who are receiving 