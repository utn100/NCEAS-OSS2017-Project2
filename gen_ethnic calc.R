# load libraries
library(dplyr)


#load csv
census_var <- read.csv("census_data.csv")


# race similarity calc : gen ethnic=1-((white/totalpop)^2+(black/totalpop)^2 + 
#(native/totalpop)^2 + (asian/totalpop)^2 + (hawaiian/totalpop)^2 + (some_other/totalpop)^2 + 
#(two_more/totalpop)^2)
# we used % of population to calculate gen ethnic for our dataset. 

#census_var[,27:34] <-NULL


ethnic <- rowSums(census_var [,c("White_Pop","Asian_Pop","American_Indian_Pop", 
                                 "Black_Pop",
                                 "Hispanic_Pop",
                                 "Two_Races_Pop")], na.rm=TRUE)


                                    

# General Education : gen education= (college - lesshigh)

education <- census_var[,"Education_Bachelor"] - 
  census_var[,"Education_High_School"]
education <- (education/100)

# Employment:  gen employ_diff= (employ - unemploy)

employ <- census_var[,"Employment_Employed"] - 
  census_var[,"Employment_Unemployed"]
employ <- (employ/100)

#Income Inequality

income_equ <- mutate(census_var, gini = Gini_index)
income_equ_only <- income_equ[,c(31)]


#Race income homogeneity

#Total income = gen inc_tlrace=(minc_white	+ minc_black + minc_native	
# + minc_asian +	minc_hawaiian +	minc_some +	minc_twomore)

#gen raceincome= 1-((minc_white/inc_tlrace)^2 + 
#(minc_black/inc_tlrace)^2 + (minc_asian/inc_tlrace)^2 + 
#(minc_native/inc_tlrace)^2 + (minc_hawaiian/inc_tlrace)^2 + 
#(minc_some/inc_tlrace)^2 + (minc_twomore/inc_tlrace)^2) 


total_inc <- rowSums(census_var [,c("two_races_income", 
                                       "American_Indian_Income",
                                       "Hispanic_Income",
                                       "White_Income",
                                       "Black_Income",
                                       "Asian_Income")])

Two_inc <- (census_var[,"two_races_income"]/total_inc)^2
Am_ind_inc <- (census_var[,"American_Indian_Income"]/total_inc)^2
Hisp_inc <- (census_var[,"Hispanic_Income"]/total_inc)^2
Wh_inc <- (census_var[,"White_Income"]/total_inc)^2
Blk_inc <- (census_var[,"Black_Income"]/total_inc)^2
Asian_inc <- (census_var[,"Asian_Income"]/total_inc)^2

race_tbl <- census_var[,c(1,4)]
race_tbl <- race_tbl %>% mutate(Two_inc, Am_ind_inc, Hisp_inc, Wh_inc, Blk_inc, Asian_inc)
race_inc <- rowSums(race_tbl [,c("Two_inc", "Am_ind_inc", "Hisp_inc", "Wh_inc", "Blk_inc", "Asian_inc")])

# gender income homogeneity

gen_inc_total <- rowSums(census_var [, c("male_household", "female_household")])
gen_inc_tbl <- census_var[,c(1,4)]
male_inc <- (census_var[,"male_household"]/gen_inc_total)^2
female_inc <- (census_var[,"female_household"]/gen_inc_total)^2
gen_inc_tbl <- gen_inc_tbl %>% mutate(male_inc, female_inc) 
gen_inc <- rowSums(gen_inc_tbl [,c("male_inc", "female_inc")])

# Communication 
# gen commu_p=(ph_owner + ph_rent)/ totalhh
# for now we're just using landlines

comms <- mutate(census_var, comm = landline_phone/100)
comms_only <- comms[,c(31)]

#Language competency 

eng_lang <- mutate(census_var, english = speaks_english/100)
eng_only <- eng_lang[,c(31)]

# 65 and older population

elder <- mutate(census_var, elders = percent_65_older/100)
elder_only <- elder[,c(31)]

#Bonding Calculation inputs 
result_tbl <- census_var[,c(1,4)]



result_tbl <- result_tbl %>% mutate(ethnic_a =(1 - (ethnic/100^2)), 
                                    elder_only,
                                    income_equ_only,
                                    employ, 
                                    education, 
                                    race_inc = (1 - (race_inc)),
                                    gen_inc = 1 - (gen_inc),
                                    eng_only,
                                    comms_only)  



# calculate Bonding score for each fips

bonds <- rowSums(result_tbl [,c("ethnic_a", 
                                    "elder_only", 
                                    "income_equ_only", 
                                    "employ", 
                                    "education",
                                    "race_inc",
                                    "gen_inc",
                                    "eng_only",
                                    "comms_only")])

bond_SoCI <- mutate(result_tbl, bond_SoCI = bonds/9)                                 

write.csv(bond_SoCI, file = "bond_SoCI.csv")
 


