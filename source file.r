rm(list = ls())
df1 = read.csv("C:/Users/HP/Downloads/Bangalore.csv")
View(df1)
dim(df1)
df = df1[,c(-3, -29, -39, -36, -37, -38, -40, -34)]
dim(df)
attach(df)
View(df)

# finding the structure of the data --
str(df)

# finding the summary of the data --
summary(df)

correlation = array(0)
for(i in 1:ncol(df)) {
  correlation[i] = round(cor(df$Price, df[i]),2)
}
correlation
# fitting the linear regreession model --

model = lm(Price ~., data = df)
summary(model) # R2 = 0.8598 adjusted R2 = 0.8575
anova(model)
plot(model, which = 1)
bptest(model)

plot(model, which = 2)
r = residuals(model)
shapiro.test(r)
plot(model, which = 3)
plot(model, which = 4)
acf(r, type = "correlation")
dwtest(model) # autocorrelation is present

# insignificant are resale, maintenancestaff, gymnasium, 
# jogging track, sportsfacility, club house, security,
# gas, ac, childplayarea.

df_n = subset(df, select = -c(Resale, MaintenanceStaff, 
                              Gymnasium, JoggingTrack, 
                              SportsFacility, ClubHouse,
                              X24X7Security, Gasconnection,
                              AC, Childrenplayarea))

summary(lm(Price ~., data = df_n))
# R2 = 0.8594 adjusted R2 = 0.8579

# separating different variables --
infrustructure = data.frame(df$Price, df$Area, df$No..of.Bedrooms, df$Resale, df$RainWaterHarvesting,
                            df$WashingMachine, df$Gasconnection, df$AC,
                            df$BED, df$VaastuCompliant)
dim(infrustructure)
amenity = data.frame(df$Price, df$MaintenanceStaff, df$Gymnasium, df$SwimmingPool, df$LandscapedGardens,
                     df$JoggingTrack, df$IndoorGames, df$Intercom, df$SportsFacility,
                     df$X24X7Security, df$PowerBackup, df$CarParking, df$StaffQuarter,
                     df$MultipurposeRoom, df$Childrenplayarea, df$LiftAvailable,
                     df$GolfCourse)
dim(amenity)
neighbourhood = data.frame(df$Price, df$ShoppingMall, df$ATM, df$ClubHouse, df$School, df$Cafeteria, 
                           df$Hospital)
dim(neighbourhood)

library(car)
library(lmtest)
# fitting the regression model in each of the dataframes --+
library(ggplot2)
library(GGally)
ggpairs(infrustructure) + theme_bw()
model_inf = lm(df.Price ~ ., data = infrustructure)
summary(model_inf) # washing machine, gasconnection, ac, bed are not significant
# R2 = 0.8401 adjusted R2 = 0.8394
anova(model_inf)
vif(model_inf) # multicollimearity is present

# test for homoscedasticity
plot(model_inf, which = 1, sub = "infrustructure Dataframe")
bptest(model_inf)

# test for normality
plot(model_inf, which = 2, sub = "infrustructure Dataframe")
r1 = residuals(model_inf)
shapiro.test(r1)

plot(model_inf, which = 3, sub = "infrustructure Dataframe")
plot(model_inf, which = 4, sub = "infrustructure Dataframe")

acf(r1, type = "correlation")
dwtest(model_inf)

# removing the insignificant variable --
inf1 = infrustructure[, c(-6, -7, -8, -9)]
summary(lm(df.Price ~ ., data = inf1)) # adjusted R^2 = 0.8395
# R2 = 0.8399
library(ggplot2)
library(GGally)
ggpairs(amenity) + theme_bw()
model_amenity = lm(df.Price ~ ., data = amenity)
summary(model_amenity) # gymnasium, intercom, childplayarea are not significant
# R2 = 0.1166 adjusted R2 = 0.1093
anova(model_amenity)
vif(model_amenity) # multicollinearity is not present

# test for homoscedasticity
plot(model_amenity, which = 1, sub = "amenity Dataframe")
bptest(model_amenity)

# test for normality
plot(model_amenity, which = 2, sub = "amenity Dataframe")
r2 = residuals(model_amenity)
shapiro.test(r2)

plot(model_amenity, which = 3, sub = "amenity Dataframe")
plot(model_amenity, which = 4, sub = "amenity Dataframe")

acf(r2, type = "correlation")
dwtest(model_amenity)

amen1 = amenity[, c(-3, -8, -15)]
summary(lm(df.Price ~ ., data = amen1)) # adjusted R^2 = 0.1098
# R2 = 0.1157

ggpairs(neighbourhood) + theme_bw()
model_neigh = lm(df.Price ~ ., data = neighbourhood)
summary(model_neigh) # shopping mall and cafeteria are not significant
# R2 = 0.04541 adjusted R2 = 0.04247
anova(model_neigh)
vif(model_neigh) # multicollinearity is not present

# test for homoscedasticity
plot(model_neigh, which = 1, sub = "neighbourhood Dataframe")
bptest(model_neigh)

# test for normality
plot(model_neigh, which = 2, sub = "neighbourhood Dataframe")
r3 = residuals(model_neigh)
shapiro.test(r3)

plot(model_neigh, which = 3, sub = "neighbourhood Dataframe")
plot(model_neigh, which = 4, sub = "neighbourhood Dataframe")

acf(r3, type = "correlation")
dwtest(model_neigh)

n1 = neighbourhood[, c(-2, -6)]
summary(lm(df.Price ~ ., data = n1)) 
# R2 = 0.04487 adjusted R2 = 0.0429