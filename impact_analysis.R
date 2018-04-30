# Impact analysis

#education_joined = import_education() %>% process_education() %>% join_education()

# Get matched_final
matched_impact = read.csv('../data/export/matched_final_less_p1_2017-06-21.csv', stringsAsFactors = F)

### Assumptions
discovered_deps = nrow(education_joined$match_2016)
pef_per_student = 1200
cg_per_student = 55

assumptions = data.frame(assumption = c("discoverd_deps", "funding per student", "clothing grant per student"), variable = c(discovered_deps, pef_per_student, cg_per_student))

### Cost of the school day
pef_total = discovered_deps * pef_per_student

### FTE staff reduction
staff_reduction = 3 * (20000 * 1.3)

### Council outlay for CG
outlay = discovered_deps * cg_per_student

### 
impact = data.frame(change = c("new_dependents", "pef_funding", "FTE staff reduction", "CG additional costs"), 
                    description = c("added through the match - count", "part of the cost of the school day program - £1,200 per student goes to schools from Scottish Government - £", "3 x staff reduced that used to manage applications - £", "Costs borne by GCC for additonal clothing grant payments"),
                    n = c(discovered_deps, pef_total, staff_reduction, outlay)
                    ) %>% arrange(-rate)

write.csv(assumptions, paste("../data/export/impact_assumptions_", Sys.Date(), ".csv", sep=""), row.names=F)
write.csv(impact, paste("../data/export/impact_", Sys.Date(), ".csv", sep=""), row.names=F)
