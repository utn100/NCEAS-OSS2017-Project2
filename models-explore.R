
library(lattice)
library(ggplot2)
library(car)

master <- read.csv("/home/shares/oss2017/social/DATA/master_20170726.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

names(master)
View(master[master$year == 2010,])

master$pop_change1 <- NA
for(i in unique(master$year)){
  master$pop_change1[which(master$year == i)] <- (master$Pop[which(master$year == (i+1))] - master$Pop[which(master$year == i)])/master$Pop[which(master$year == i)]
}

master$pop_change2 <- NA
for(i in unique(master$year)){
  master$pop_change2[which(master$year == i)] <- (master$Pop[which(master$year == (i+2))] - master$Pop[which(master$year == i)])/master$Pop[which(master$year == i)]
}

master$perc_total_dev <- master$perc_dev_high + master$perc_dev_med + master$perc_dev_low
master$perc_total_est_wet <- with(master, perc_est_forest_wet +
                                    perc_est_scrub_wet +
                                    perc_est_emerg_wet)

master2010 <- master[master$year == 2010,]
master2010$STATEFP <- as.factor(master2010$STATEFP)


#check the distribution to see how not normal the y variable is:
hist(master$pop_change, breaks = 20)
#Not too bad. Let's go with it.

par(mar = c(4, 4, 4, 4))
plot(master$mean.el, master$Pop, col = "blue")
hist(master$Pop)
hist(master$mean.el)
class(master$Pop)
master$Pop <- as.numeric(master$Pop)

# EXPLORE VARIABLES ####
splom(~ master[,c(64, 41, 8, 27, 14, 63)])
plot(master[,41], master[,63],
     xlab ="sea level",
     ylab = "bonding")

ggplot(master, aes(Sea_level.mm, bond_SoCI, col = factor(year))) +
  geom_point()
#
ggplot(master, aes(perc_pal_emerg_wet, perc_dev_high)) +
  geom_point()

m1 <- glm(pop_change ~ Sea_level.mm +
            Mean.Temp.K +
            perc_pal_emerg_wet +
            perc_dev_high +
            bond_SoCI, 
          data = master)
summary(m1)

summary(master2010$pop_change1)
summary(master2010$pop_change2)

plot(master2010$SoCI_bond, master2010$pop_change1)
plot(master2010$SoCI_bond, master2010$pop_change2)

plot(master2010$Sea_level.mm, master2010$pop_change1)
plot(master2010$Sea_level.mm, master2010$pop_change2)

m2 <- glm(pop_change2 ~ Sea_level.mm*SoCI_bond, data = master2010)
summary(m2)
Anova(m2)

plot(plot(master2010$Wind.speed.m.s, master2010$pop_change1))
hist(master2010$Wind.speed.m.s)

pdf("data-explore.pdf")
par(mfrow = c(2, 2))
for(i in c(3, 4, 6:41, 44:62)){
  plot(master2010[,i], master2010$pop_change1,
       xlab = names(master2010)[i],
       ylab = "% Population Change to Next Year",
       bty = "l",
       pch = 16, col = rgb(0, 0, 0, 1/4))
  abline(h = 0, lty = 2, lwd = 2)
}
dev.off()

pdf("data-explore2.pdf")
par(mfrow = c(2, 2))
for(i in c(3, 4, 6:41, 44:62)){
  plot(master2010[,i], master2010$pop_change2,
       xlab = names(master2010)[i],
       ylab = "% Population Change to Next 2 Years",
       bty = "l",
       pch = 16, col = rgb(0, 0, 0, 1/4))
  abline(h = 0, lty = 2, lwd = 2)
}
dev.off()

m3 <- glm(pop_change1 ~ SoCI_bond, 
            data = master2010[which(master2010$pop_change2 > 0.02),])

m4 <- glm(pop_change2 ~ SoCI_bond, 
            data = master2010[which(master2010$pop_change2 < 0.05),])


m5 <- glm(pop_change1 ~ SoCI_bond * Sea_level.mm,
          data = master2010[which(master2010$pop_change2 > 0.02),])
summary(m5)
Anova(m5, type = "III")

m5.2 <- glm(pop_change1 ~ (SoCI_bond + SoCI_bridge) * 
              (Sea_level.mm + Mean.Precipitation.kg.m.2 + perc_total_dev + perc_total_est_wet),
            data = master2010)
summary(m5.2)
Anova(m5.2, type = "III")

m5.3 <- glm(pop_change2 ~ (SoCI_bond + SoCI_bridge) * 
              (Sea_level.mm + Mean.Precipitation.kg.m.2 + perc_total_dev + perc_total_est_wet),
            data = master2010)
summary(m5.3)
Anova(m5.3, type = "III")

plot(master2010$perc_total_dev, master2010$perc_total_est_wet)

#work with 1 year lag:
m5.4 <- glm(pop_change1 ~ (SoCI_bond + SoCI_bridge) * perc_total_est_wet +
              Sea_level.mm + Mean.Precipitation.kg.m.2,
            data = master2010)
summary(m5.4)
Anova(m5.4, type = "III")

m5.5 <- glm(pop_change1 ~ (SoCI_bond + SoCI_bridge) * perc_total_est_wet +
              Sea_level.mm,
            data = master2010)
summary(m5.5)
Anova(m5.5, type = "III")

m5.6 <- glm(pop_change1 ~ (SoCI_bond + SoCI_bridge) * 
              Sea_level.mm,
            data = master2010)
summary(m5.6)
Anova(m5.6, type = "III")

m5.7 <- glm(pop_change1 ~ SoCI_bond * (perc_total_est_wet) + SoCI_bridge,
            data = master2010)
summary(m5.7)
Anova(m5.7, type = "III")


#make some plots
ggplot(master2010, 
       aes(SoCI_bond, pop_change1, col = perc_total_est_wet)) +
  geom_point()

ggplot(master2010, 
       aes(SoCI_bond, pop_change1, col = Sea_level.mm)) +
  geom_point()

ggplot(master2010, 
       aes(SoCI_bond, pop_change1, col = max.el)) +
  geom_point()


# master$urban <- master$perc_dev_high + 
#   master$perc_dev_low + 
#   master$perc_dev_med +
#   master$perc_open_sp_dev
# hist(master$urban)
