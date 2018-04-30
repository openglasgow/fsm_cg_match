## Analysis of households with new children - how many are there? 

# Establish cut off dates (1 yo to 2.5 yo)
start_cutoff = seq(Sys.Date(), by = "-32 months", length = 2)[2]
end_cutoff = seq(Sys.Date(), by = "-12 months", length = 2)[2]

# slice members
members_toddlers = education_joined$members %>% filter(birth_date >= start_cutoff & birth_date <= end_cutoff) 

# join with households
toddler_households = left_join(members_toddlers, education_joined$households, by=c("claim_id"))

# count and select
toddler_count = toddler_households  %>% filter(type=="Child Under 16") %>% group_by(claim_id) %>% summarize(toddler_count = n())
