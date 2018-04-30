### Add FSM criteria to matched cases
matched_final = matched_final %>% mutate(hb_id_member = paste(hb_id, hb_member))

### 1. get our ids from incomes
cohort_income_ids = education_norm$income %>% filter(claim_id %in% matched_final$hb_id) %>% select(claim_id) %>% unique() %>% .$claim_id

### 2. on JSA or income suppport or child tax credit
cohort_support_ids = education_norm$incomes %>% filter(claim_id %in% cohort_income_ids & (inc_code %in% c("JSI", "CTX", "INS"))) %>% select(claim_id) %>% unique() %>% .$claim_id

### 3. on WTX with less than £6420 or is it wtx less than that / year? 
cohort_wtx_ids = extract$income %>% 
  filter(claim_id %in% cohort_income_ids) %>% 
  group_by(claim_id, inc_code) %>% 
  summarize(n=n(), amt=sum(weekly_income_calculated)) %>% 
  filter(inc_code=="WTX") %>% 
  mutate(yearly_sum=amt*52) %>% 
  filter(yearly_sum<=6420) %>% select(claim_id) %>% unique() %>% .$claim_id

cohort_deps_fsm_eligible = cohort %>% filter(dwp_claim_id %in% c(cohort_support_ids, cohort_wtx_ids))

### 4. For EMA - remove children under 16
cohort_over = cohort %>% filter(dep_dob <= Sys.Date()-(16*365))

### 5. No earned income
cohort_ema_no_earned_ids = extract$income %>% filter(claim_id %in% cohort_over$dwp_claim_id & !(inc_code %in% c("EAR"))) %>% select(claim_id) %>% unique() %>% .$claim_id

### 6. WTX ids
cohort_ema_wtx_ids = extract$income %>% filter(claim_id %in% cohort_over$dwp_claim_id & inc_code %in% c("WTX")) %>% select(claim_id) %>% unique() %>% .$claim_id

### 7. WTX - all income for the houses on some kind of wtx
cohort_ema_wtx = extract$income %>% 
  filter(claim_id %in% cohort_ema_wtx_ids) %>% 
  group_by(claim_id) %>% 
  summarise(count=n(), amt = sum(weekly_income_calculated)) %>% 
  mutate(yearly_sum = amt*52) %>% 
  merge(extract$income, ., by="claim_id")


### Try something new = add income information to our cohort document
cohort_merged = left_join(cohort, (extract$income %>% group_by(claim_id) %>% summarize(yearly_household_income = sum(weekly_income_calculated)*52)), by=c("dwp_claim_id" = "claim_id")) %>% 
  left_join(., (extract$income %>% group_by(claim_id) %>% summarize(income_codes = paste(inc_code, collapse=","))), by=c("dwp_claim_id" = "claim_id")) %>%
  mutate(WTX_eligible = as.vector(!is.na(str_match(income_codes, "WTX"))), 
         JSA_elligible = as.vector(!is.na(str_match(income_codes, "JSI"))), 
         CTX_eligible = as.vector(!is.na(str_match(income_codes, "CTX"))),
         IS_eligible = as.vector(!is.na(str_match(income_codes, "INS"))),
         earned_income = as.vector(!is.na(str_match(income_codes, "EAR"))),
         under_16 = dep_dob <= Sys.Date()-(16*365),
         income_under_6420 = yearly_household_income <= 6420,
         income_under_26_5k = yearly_household_income <= 26500,
         born_between_1mar1997_28feb_2001 = (dep_dob >= as.Date('1997-03-1') & dep_dob <= as.Date('2001-02-28')))

### Add information to our matched sheet

# Income Support or Jobseekers Allowance
# Working Tax Credit with a pre tax household income of £6420 or lower
# Child Tax Credit with a pre tax household income of 16105
# Universal Credit (less than £500 per moth)

#1. Build income criteria
incomes_household = education_norm$incomes[] %>% 
  group_by(claim_id) %>% 
  summarize(income_codes = paste(inc_code, collapse=","), yearly_household_income=sum(weekly_income_calculated)*52) %>% 
  mutate(WTX_elligible = as.vector(!is.na(str_match(income_codes, "wtx"))),
         JSA_elligible = as.vector(!is.na(str_match(income_codes, "jsi"))), 
         CTX_eligible = as.vector(!is.na(str_match(income_codes, "ctx"))),
         IS_eligible = as.vector(!is.na(str_match(income_codes, "ins"))),
         IS_eligible = as.vector(!is.na(str_match(income_codes, "esa"))),
         earned_income = as.vector(!is.na(str_match(income_codes, "ear"))),
         claim_id = as.character(claim_id))

matched_final = left_join(matched_final, incomes_household, by=c("hb_id" = "claim_id")) %>% 
  mutate(hb_dob = as.Date(paste(HB_dob_y, HB_dob_m, HB_dob_d, sep="-"))) %>% 
  mutate(under_16 = hb_dob <= Sys.Date()-(16*365),
         income_under_6420 = yearly_household_income <= 6420,
         income_under_26_5k = yearly_household_income <= 26500,
         born_between_1mar1997_28feb_2001 = (hb_dob >= as.Date('1997-03-1') & hb_dob <= as.Date('2001-02-28')))

### *** Mel analysis mark 2 ***
## Bring matched final over from "run.R"

