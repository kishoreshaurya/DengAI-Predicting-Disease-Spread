library(readr)
library(tidyverse)
library(mice)
library(glmnet)
library(corrplot)
install.packages("pls")
library(pls)

train <- read_csv("dengue_features_train (2).csv")
dengue_features_test <- read_csv("dengue_features_test.csv")
dengue_features_test$total_cases <- NA
main_df <- rbind(train, dengue_features_test)
head(main_df)


#missing value analaysis
md.pattern(main_df)
colSums(is.na(main_df))
sum.na <- sort(sapply(main_df, function(x) { sum(is.na(x))}), decreasing=TRUE)
sum.na
sum.na.percent <- sort(sapply(main_df, function(x) { sum(is.na(x)/dim(main_df)[1])}), decreasing=TRUE)
as.data.frame(sum.na.percent)

df <- as.data.frame(main_df[,25])
main_df <- main_df[,-c(25)]

#imputation
imputed_data <- mice(main_df, m = 5, method = "cart")
main_df <- mice::complete(imputed_data)
options(na.action='na.fail')
main_df <-  cbind(main_df, df )

main_df %>% drop_na(reanalysis_sat_precip_amt_mm)
main_df$reanalysis_sat_precip_amt_mm <- NULL

#Check correlation
contVar <- names(main_df)[which(sapply(main_df, is.numeric))]
trainCont <- main_df[, contVar]
correlations <- cor(trainCont, use = "pairwise.complete.obs")
corrplot(correlations, method = "square")

# look up top 10 feature correlated to total cases
cor <- as.data.frame(as.table(correlations))
cor <- subset(cor, cor$Var2 == "total_cases")
cor <- cor[order(cor$Freq, decreasing = T)[1:10],]
cor

#outlier analysis
plot(main_df$reanalysis_dew_point_temp_k, main_df$total_cases)

#Skewness
df <- as.data.frame(main_df[,24])
main_df <- main_df[,-c(24)]

library(e1071)

classes <- lapply(main_df,function(x) class(x))
numeric_feats <- names(classes[classes=="integer" | classes=="numeric"])
factor_feats <- names(classes[classes=="factor"| classes=="character"])

skewed_feats <- sapply(numeric_feats, function(x) skewness(main_df[[x]]))
skewed_feats <- skewed_feats[abs(skewed_feats) > .50]
as.table(skewed_feats)
hist(main_df$reanalysis_precip_amt_kg_per_m2)

for (x in names(skewed_feats)) {main_df[[x]] <- log(main_df[[x]]+1)}
main_df <-  cbind(main_df, df )
colnames(main_df)[24] <- "total_cases"

main_df$city <- factor(main_df$city)
main_df$weekofyear <- factor(main_df$weekofyear)
str(main_df)

train_df <- main_df[1:1456,]
test_df <- main_df[1457:1872,]

denguecasesregion<-aggregate(total_cases~year+city,train_df,sum)


#Temporal Spatial Heatmap
ggplot(denguecasesregion, aes(x= year, y = total_cases)) + geom_line()
ggplot(train_df, aes(x= week_start_date, y = total_cases)) + geom_line()

#divide dataser into two cities
ts_df <- train_df[c(-1431),c(3,4,24)]
ts_df_sj <- train_df %>% filter(city == "sj") 
ts_df_sj <- ts_df_sj[,c(1,3,4,24)]

ts_df_iq <- train_df %>% filter(city == "iq") 
ts_df_iq <- ts_df_iq[c(-1431),c(1,3,4,24)]

ts_df_iq <- ts_df_iq %>% filter(ts_df_iq$week_start_date >= "2000-07-01" & ts_df_iq$week_start_date <= "2008-04-22")
ts_df_sj <- ts_df_sj %>% filter(ts_df_sj$week_start_date >= "2000-07-01" & ts_df_sj$week_start_date <= "2008-04-22")               

ts_df_main <- cbind(ts_df_sj, ts_df_iq)

ts_df_main <-aggregate(total_cases~week_start_date+weekofyear,ts_df_main,sum)

ts_df_main$month <- format(ts_df_main$week_start_date,"%B")
ts_df_main$year <- format(ts_df_main$week_start_date,"%Y")

ts_df_main$month <- as.factor(ts_df_main$month)
str(ts_df_main)
library(dplyr)
ts_df_main <- arrange(ts_df_main, week_start_date)
ts_df_main$year <- as.integer(ts_df_main$y)


denguecasesph_main<-aggregate(total_cases~month+year+weekofyear+week_start_date,ts_df_main,sum)
denguecasesph_main$year <- as.integer(denguecasesph_main$year)
str(denguecasesph_main)


denguecasesphts_main<-ts(denguecasesph_main$total_cases,c(2000,26),c(2008,17),52)
autoplot(denguecasesphts_main)+xlab(label = "TIME")

autoplot(decompose(denguecasesphts_main))
adf.test((denguecasesphts_main))


dengueph_arima_combined<-auto.arima(denguecasesphts_main)
dengueph_arima_combined
plot(residuals(dengueph_arima_combined))
checkresiduals(dengueph_arima_combined)

dengueph_forecast<-forecast(dengueph_arima_combined,52)

autoplot(dengueph_forecast)+
  ylab(label = "DENGUE CASES")
