### Import sets

import_education = function(){

  cat("Importing hb members \n")
  members = read.delim("../data/import/hb_member_2016_05_15.txt", stringsAsFactors = FALSE, header=FALSE, sep="|", strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("Importing hb incomes \n")
  incomes = read.delim("../data/import/hb_income_2017_05_15.txt", stringsAsFactors = FALSE, header=FALSE, sep="|", strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("Importing hb household \n")
  households = read.delim("../data/import/hb_household_2017_05_15.txt", stringsAsFactors = FALSE, header=FALSE, sep="|", strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("Importing School Roll \n")
  school_roll = read.csv("../data/import/scholl_roll_2017_05_23.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("importing last years results\n")
  match_2016 = read.csv("../data/import/dwp_final_cohort_2016-11-28.csv", stringsAsFactors = FALSE, strip.white=TRUE, na.strings=c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  cat("importing this years p1's\n")
  p1 = read.csv("../data/import/p1_2017_06_05.csv", stringsAsFactors = FALSE, strip.white=TRUE, na.strings=c("", "^\\s+$", "N/A", "N / A", "na", "n/a", "n / a"))
  return(list(households=households, members=members, incomes=incomes, school_roll = school_roll, p1 = p1, match_2016=match_2016))
}