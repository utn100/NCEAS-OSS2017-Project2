# Calculation of Social Capital Bridging Values

#Load bridging calculation raw data

bridge_var <- read.csv("bridging_variables.csv")


# Get statistics for the variables by year

SoCI_bridge_stats <- bridge_var %>% group_by(year) %>% 
  summarize_at(vars(Religion:Civic), c("mean", "max", "min"), na.rm = TRUE)


SoCI_bridge_combo <- left_join(bridge_var, SoCI_bridge_stats, by = "year")

#normalization of yearly data

SoCI__bridge_norm <- SoCI_bridge_combo %>% mutate (religion_soc = (Religion - Religion_min)/(Religion_max - Religion_min),
                                        civic_soc = (Civic -Civic_min)/(Civic_max - Civic_min),
                                        affiliate_soc = (Affiliate - Affiliate_min)/(Affiliate_max - Affiliate_min))


SoCI_bridge_2 <- mutate(SoCI__bridge_norm, SoCI_bridge = ((religion_soc + civic_soc + 
                                                 affiliate_soc)/3))

# Pull out final table with just fips, year and bridging number

SoCI_bridge_2010 <- select(SoCI_bridge_2, c(fips_n, year, SoCI_bridge))

View(SoCI_bridge_2010)


# write final csv
write.csv(SoCI_bridge_2010, file = "SoCI_bridge_2010.csv")
