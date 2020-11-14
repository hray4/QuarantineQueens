#cleaning CDC Underlying conditions data set for analysis - imported data set as conditions_covid

library(tidyverse)
library(lubridate)




#Removing all records which are flagged by CDC for confidentiality reasons and removing the empty Flag column

conditions_no_flags <- subset(conditions_covid, (Flag != 'One or more data cells have counts between 1-9 and have been suppressed in accordance with NCHS confidentiality standards.'))

conditions_no_flags <- subset(conditions_no_flags, select = -c(Flag))

conditions_no_flags <- subset(conditions_no_flags, select = -c(Data.as.of)) #Also removing the 'Data as of' column since it is just marking the cut-off data

conditions_no_flags$Start.Week <- mdy(conditions_no_flags$Start.Week) #fixing data type

conditions_no_flags$End.Week <- mdy(conditions_no_flags$End.Week) #fixing data type

glimpse(conditions_no_flags)





#Creating version where we only have age ranges and removing records where age is unknown

unaggregated_age_conditions_covid <- subset(conditions_no_flags, (Age.Group != "All Ages" & Age.Group != "Not stated"))
glimpse(unaggregated_age_conditions_covid)




#Creating version with only age totals
aggregated_age_conditions_covid <- subset(conditions_no_flags, (Age.Group == "All Ages"))




#Overall USA counts only with aggregated age
usa_aggregated_age_conditions_covid <- subset(aggregated_age_conditions_covid, (State == "US"))



#State counts with aggregated age
state_aggregated_age_conditions_covid <- subset(aggregated_age_conditions_covid, (State != "US"))



#Overall USA counts with unaggregated age
usa_unaggregated_age_conditions_covid <- subset(unaggregated_age_conditions_covid, (State == "US"))




#State counts with unaggregated age
state_unaggregated_age_conditions_covid <- subset(unaggregated_age_conditions_covid, (State != "US"))
glimpse(state_unaggregated_age_conditions_covid)




#creating version of all data sets which ONLY have the conditions with STRONG EVIDENCE behind the relationship per the CDC summary table
#Strong Evidence conditions from that table include cancer, chronic kidney disease, COPD, heart conditions, obesity and severe obesity, pregnancy, sickle cell disease, smoking, solid organ transplants, and type 2 diabetes.
#Limited the data to conditions which matched those with Strong Evidence, including CLRD (encompasses COPD), cardiac arrest, heart failure, ischemic heart disease, cardiac arrhythmia, diabetes, obesity, and renal failure (related to CKD)
#Should note limitation that diabetes type is NOT defined in this data set and ONLY type 2 has strong evidence
#COVID-19 totals are retained

unique(conditions_no_flags$Condition)

SE_conditions_no_flags <- subset(conditions_no_flags, 
                                (Condition == "Chronic lower respiratory diseases" | 
                                   Condition == "Ischemic heart disease" | 
                                   Condition == "Cardiac arrest" | 
                                   Condition == "Cardiac arrhythmia" | 
                                   Condition == "Heart failure" | 
                                   Condition == "Diabetes" | 
                                   Condition == "Obesity" | 
                                   Condition == "COVID-19" |
                                   Condition == "Renal failure"))

SE_usa_aggregated_age_conditions_covid <- subset(usa_aggregated_age_conditions_covid, 
                                                 (Condition == "Chronic lower respiratory diseases" | 
                                                    Condition == "Ischemic heart disease" | 
                                                    Condition == "Cardiac arrest" | 
                                                    Condition == "Cardiac arrhythmia" | 
                                                    Condition == "Heart failure" | 
                                                    Condition == "Diabetes" | 
                                                    Condition == "Obesity" | 
                                                    Condition == "COVID-19" | 
                                                    Condition == "Renal failure"))

SE_usa_unaggregated_age_conditions_covid <- subset(usa_unaggregated_age_conditions_covid, 
                                                 (Condition == "Chronic lower respiratory diseases" | 
                                                    Condition == "Ischemic heart disease" | 
                                                    Condition == "Cardiac arrest" | 
                                                    Condition == "Cardiac arrhythmia" | 
                                                    Condition == "Heart failure" | 
                                                    Condition == "Diabetes" | 
                                                    Condition == "Obesity" | 
                                                    Condition == "COVID-19" | 
                                                    Condition == "Renal failure"))

SE_state_unaggregated_age_conditions_covid <- subset(state_unaggregated_age_conditions_covid, 
                                                   (Condition == "Chronic lower respiratory diseases" | 
                                                      Condition == "Ischemic heart disease" | 
                                                      Condition == "Cardiac arrest" | 
                                                      Condition == "Cardiac arrhythmia" | 
                                                      Condition == "Heart failure" | 
                                                      Condition == "Diabetes" | 
                                                      Condition == "Obesity" | 
                                                      Condition == "COVID-19" | 
                                                      Condition == "Renal failure"))

SE_state_aggregated_age_conditions_covid <- subset(state_aggregated_age_conditions_covid, 
                                                     (Condition == "Chronic lower respiratory diseases" | 
                                                        Condition == "Ischemic heart disease" | 
                                                        Condition == "Cardiac arrest" | 
                                                        Condition == "Cardiac arrhythmia" | 
                                                        Condition == "Heart failure" | 
                                                        Condition == "Diabetes" | 
                                                        Condition == "Obesity" | 
                                                        Condition == "COVID-19" | 
                                                        Condition == "Renal failure"))

#Write csv files
write.csv(conditions_no_flags, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_filtered_missing_data.csv", row.names = FALSE)
write.csv(unaggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_age_ranges.csv", row.names = FALSE)
write.csv(aggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_age_totals.csv", row.names = FALSE)
write.csv(usa_aggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_USA_age_totals.csv", row.names = FALSE)
write.csv(usa_unaggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_USA_age_ranges.csv", row.names = FALSE)
write.csv(state_aggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_state_age_totals.csv", row.names = FALSE)
write.csv(state_unaggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_state_age_ranges.csv", row.names = FALSE)
write.csv(SE_conditions_no_flags, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_filtered_missing_data_strong.csv", row.names = FALSE)
write.csv(SE_usa_aggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_USA_age_totals_strong.csv", row.names = FALSE)
write.csv(SE_state_aggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_state_age_totals_strong.csv", row.names = FALSE)
write.csv(SE_state_unaggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_state_age_ranges_strong.csv", row.names = FALSE)
write.csv(SE_usa_unaggregated_age_conditions_covid, "C:/Users/Ray/Documents/Coding/CS504/Group Project/Cleaned_data/UC_USA_age_ranges_STRONG.csv", row.names = FALSE)
