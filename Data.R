# load libraries
library(readabs)
R_READABS_PATH = "C:/Users/PaulWard/GitHub/Energy_Poverty"
# set path for ABS files to local drive
library(tidyverse)
library(lubridate)
library(zoo)
library(readxl)
library(modelr)
library(dynlm)
library(MASS, exclude = "select")
library(lmtest)

options(digits=20)
# set number of decimal points to 20 (needed for the AER data)


e_syd <- read_abs(series_id = "A2328101R")
c_syd <- read_abs(series_id = "A2325806K")
# Loads raw ABS data into DF - ready for manipulation
# EPI = electricity price index

# electricity CPI (Australia) ABS Series ID = A2328141J
# electricity CPI (Sydney) ABS Series ID = A2328101R
# electricity CPI (Brisbane) ABS Series ID = A2328111V
# electricity CPI (Adelaide) ABS Series ID = A2328116F
# electricity CPI (Hobart) ABS Series ID = A2328126K
# electricity CPI (Canberra) ABS Series ID = A2328136R

# CPI (Australia) ABS Series ID = A2325846C
# CPI (Sydney) ABS Series ID = A2325806K
# CPI (Brisbane) ABS Series ID = A2325816R
# CPI (Adelaide) ABS Series ID = A2325821J
# CPI (Hobart) ABS Series ID = A2325831L
# CPI (Canberra) ABS Series ID = A2325841T


# return a NSW CPI DF
cpi_nsw <- c_syd %>%
  select(date, value)
cpi_nsw$quarter <- as.yearqtr(cpi_nsw$date, "%Y-$m-%d")
cpi_nsw <- cpi_nsw %>%
  select(quarter, value) %>%
  filter(quarter >= "2015 Q3" & quarter <= "2023 Q1")
colnames(cpi_nsw) <- c("quarter", "CPI_NSW")


# return NSW EPI DF
epi_nsw <- e_syd %>%
  select(date, value)
epi_nsw$quarter <- as.yearqtr(epi_nsw$date, "%Y-%m-%d")
epi_nsw <- epi_nsw %>%
  select(quarter, value) %>%
  filter(quarter >= "2015 Q3" & quarter <= "2023 Q1")
colnames(epi_nsw) <- c("quarter", "EPI_NSW")


# create a COVID-19 binary variable
covid <- epi_nsw %>%
  select(quarter)
covid$covid <- as.integer(c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0))



# script to load AER URL data (obsolete)
# aer_url <- read_excel("aer_url.xlsx")

# returns a DF with AER URLs for loading hardship program percentage data
aer_url <- as.data.frame(c("AER01", "AER02", "AER03", "AER04", "AER05", "AER06", "AER07", "AER08","AER09"))
aer_url$period <- c("2015-16",	"2016-17",	"2017-18",	"2018-19",	"2019-20",	"2020-21",	"2021-22",	"2022-23", "2023-24")
aer_url$Link <- c("https://www.aer.gov.au/system/files/Schedule%204%20-%20Quarter%202%202022-23%20retail%20performance%20data.xlsm", #AER06 Q2 21/23 - Q2 22/23
  "https://www.aer.gov.au/system/files/Schedule%204%20-%20Retail%20Performance%20Data%20Q1%202022-23%20%28Final%29.xlsm", #AER07 Q1 21/22 - Q1 22/23
                  "https://www.aer.gov.au/system/files/Schedule%204%20-%20Quarter%202%202022-23%20retail%20performance%20data.xlsm", #AER08 Q2 21/23 - Q2 22/23
                  "https://www.aer.gov.au/system/files/2024-06/Schedule%204%20-%20Quarter%203%202023-24%20Retail%20Performance%20Data.xlsm" #AER09 Q1 22/23 - Q3 23/24
                  )
colnames(aer_url) <- c("file_name","period","Link")


# scripts to return AER DFs for conversion:
temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[1]
download.file(dataURL, destfile = temp, mode = "wb")
AER01 <- read_excel(temp, sheet = 17, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[2]
download.file(dataURL, destfile = temp, mode = "wb")
AER02 <- read_excel(temp, sheet = 17, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[3]
download.file(dataURL, destfile = temp, mode = "wb")
AER03 <- read_excel(temp, sheet = 17, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[4]
download.file(dataURL, destfile = temp, mode = "wb")
AER04 <- read_excel(temp, sheet = 2, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[5]
download.file(dataURL, destfile = temp, mode = "wb")
AER05 <- read_excel(temp, sheet = 2, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[6]
download.file(dataURL, destfile = temp, mode = "wb")
AER06 <- read_excel(temp, sheet = 2, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[7]
download.file(dataURL, destfile = temp, mode = "wb")
AER07 <- read_excel(temp, sheet = 2, range = "A4:K300", col_names = TRUE)

temp <- tempfile(fileext = ".xlsm")
dataURL <- aer_url$Link[8]
download.file(dataURL, destfile = temp, mode = "wb")
AER08 <- read_excel(temp, sheet = 2, range = "A4:K300", col_names = TRUE)



# Converting raw AER data into individual DFs with just the data of interest

# AER1 - with explanatory notes
# select rows of interest in a DF
AER01.1 <- AER01 %>%
  filter(National %in% c("Retailer", "Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))

# remove unwanted columns
AER01.1 <- AER01.1 %>%
  select(-c(2, 3, 4, 5, 6))

# transpose rows to columns
AER1 <- as.data.frame(t(AER01.1), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER1) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER1 <- AER1 %>%
  filter(!row_number() %in% 1)

# new yearqtr column
AER1$quarter <- seq(as.yearqtr("2015 Q3"), as.yearqtr("2016 Q3"), by = 1/4)

# convert V2 in percentage INT
AER1$NatPer <- as.numeric(AER1$NatPer)
AER1$ACTPer <- as.numeric(AER1$ACTPer)
AER1$NSWPer <- as.numeric(AER1$NSWPer)
AER1$QLDPer <- as.numeric(AER1$QLDPer)
AER1$SAPer <- as.numeric(AER1$SAPer)
AER1$TASPer <- as.numeric(AER1$TASPer)

# Create number per 100K from percentage
AER1$NAT100K <- AER1$NatPer * 100000
AER1$ACT100K <- AER1$ACTPer * 100000
AER1$NSW100K <- AER1$NSWPer * 100000
AER1$QLD100K <- AER1$QLDPer * 100000
AER1$SA100K <- AER1$SAPer * 100000
AER1$TAS100K <- AER1$TASPer * 100000

# move yearqtr column to the LHS
AER1 <- AER1 %>% relocate(quarter)


# AER2
AER02.2 <- AER02 %>%
  filter(National %in% c("Retailer", "Total", "Total ACT",
                         "Total NSW", "Total QLD", "Total SA", "Total TAS"))
AER02.2 <- AER02.2 %>%
  select(-c(2, 3, 4, 5, 6))
AER2 <- as.data.frame(t(AER02.2), row.names = c(1,2,3,4,5,6))
colnames(AER2) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER2 <- AER2 %>%
  filter(!row_number() %in% 1)
AER2$quarter <- seq(as.yearqtr("2016 Q3"), as.yearqtr("2017 Q3"), by = 1/4)
AER2$NatPer <- as.numeric(AER2$NatPer)
AER2$ACTPer <- as.numeric(AER2$ACTPer)
AER2$NSWPer <- as.numeric(AER2$NSWPer)
AER2$QLDPer <- as.numeric(AER2$QLDPer)
AER2$SAPer <- as.numeric(AER2$SAPer)
AER2$TASPer <- as.numeric(AER2$TASPer)
AER2$NAT100K <- AER2$NatPer * 100000
AER2$ACT100K <- AER2$ACTPer * 100000
AER2$NSW100K <- AER2$NSWPer * 100000
AER2$QLD100K <- AER2$QLDPer * 100000
AER2$SA100K <- AER2$SAPer * 100000
AER2$TAS100K <- AER2$TASPer * 100000
AER2 <- AER2 %>% relocate(quarter)

# AER3
AER03.3 <- AER03 %>%
  filter(National %in% c("Retailer", "Total"))
AER03.3 <- AER03.3 %>%
  select(-c(2, 3, 4, 5, 6))
AER03.3$National[2] <- "NatPer"
AER03.3$National[3] <- "ACTPer"
AER03.3$National[4] <- "NSWPer"
AER03.3$National[5] <- "QLDPer"
AER03.3$National[6] <- "SAPer"
AER03.3$National[7] <- "TASPer"
AER3 <- as.data.frame(t(AER03.3), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER3) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER3 <- AER3 %>%
  filter(!row_number() %in% 1)
AER3$quarter <- seq(as.yearqtr("2017 Q3"), as.yearqtr("2018 Q3"), by = 1/4)
AER3$NatPer <- as.numeric(AER3$NatPer)
AER3$ACTPer <- as.numeric(AER3$ACTPer)
AER3$NSWPer <- as.numeric(AER3$NSWPer)
AER3$QLDPer <- as.numeric(AER3$QLDPer)
AER3$SAPer <- as.numeric(AER3$SAPer)
AER3$TASPer <- as.numeric(AER3$TASPer)
AER3$NAT100K <- AER3$NatPer * 100000
AER3$ACT100K <- AER3$ACTPer * 100000
AER3$NSW100K <- AER3$NSWPer * 100000
AER3$QLD100K <- AER3$QLDPer * 100000
AER3$SA100K <- AER3$SAPer * 100000
AER3$TAS100K <- AER3$TASPer * 100000
AER3 <- AER3 %>% relocate(quarter)

# AER4
AER04.4 <- AER04 %>%
  filter(National %in% c("Retailer", "National Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))
AER04.4 <- AER04.4 %>%
  select(-c(2, 3, 4, 5, 6))
AER4 <- as.data.frame(t(AER04.4), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER4) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER4 <- AER4 %>%
  filter(!row_number() %in% 1)
AER4$quarter <- seq(as.yearqtr("2018 Q3"), as.yearqtr("2019 Q3"), by = 1/4)
AER4$NatPer <- as.numeric(AER4$NatPer)
AER4$ACTPer <- as.numeric(AER4$ACTPer)
AER4$NSWPer <- as.numeric(AER4$NSWPer)
AER4$QLDPer <- as.numeric(AER4$QLDPer)
AER4$SAPer <- as.numeric(AER4$SAPer)
AER4$TASPer <- as.numeric(AER4$TASPer)
AER4$NAT100K <- AER4$NatPer * 100000
AER4$ACT100K <- AER4$ACTPer * 100000
AER4$NSW100K <- AER4$NSWPer * 100000
AER4$QLD100K <- AER4$QLDPer * 100000
AER4$SA100K <- AER4$SAPer * 100000
AER4$TAS100K <- AER4$TASPer * 100000
AER4 <- AER4 %>% relocate(quarter)

# AER5
AER05.5 <- AER05 %>%
  filter(National %in% c("Retailer", "National Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))
AER05.5 <- AER05.5 %>%
  select(-c(2, 3, 4, 5, 6))
AER5 <- as.data.frame(t(AER05.5), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER5) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER5 <- AER5 %>%
  filter(!row_number() %in% 1)
AER5$quarter <- seq(as.yearqtr("2019 Q3"), as.yearqtr("2020 Q3"), by = 1/4)
AER5$NatPer <- as.numeric(AER5$NatPer)
AER5$ACTPer <- as.numeric(AER5$ACTPer)
AER5$NSWPer <- as.numeric(AER5$NSWPer)
AER5$QLDPer <- as.numeric(AER5$QLDPer)
AER5$SAPer <- as.numeric(AER5$SAPer)
AER5$TASPer <- as.numeric(AER5$TASPer)
AER5$NAT100K <- AER5$NatPer * 100000
AER5$ACT100K <- AER5$ACTPer * 100000
AER5$NSW100K <- AER5$NSWPer * 100000
AER5$QLD100K <- AER5$QLDPer * 100000
AER5$SA100K <- AER5$SAPer * 100000
AER5$TAS100K <- AER5$TASPer * 100000
AER5 <- AER5 %>% relocate(quarter)

# AER6
AER06.6 <- AER06 %>%
  filter(National %in% c("Retailer", "National Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))
AER06.6 <- AER06.6 %>%
  select(-c(2, 3, 4, 5, 6))
AER6 <- as.data.frame(t(AER06.6), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER6) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER6 <- AER6 %>%
  filter(!row_number() %in% 1)
AER6$quarter <- seq(as.yearqtr("2020 Q3"), as.yearqtr("2021 Q3"), by = 1/4)
AER6$NatPer <- as.numeric(AER6$NatPer)
AER6$ACTPer <- as.numeric(AER6$ACTPer)
AER6$NSWPer <- as.numeric(AER6$NSWPer)
AER6$QLDPer <- as.numeric(AER6$QLDPer)
AER6$SAPer <- as.numeric(AER6$SAPer)
AER6$TASPer <- as.numeric(AER6$TASPer)
AER6$NAT100K <- AER6$NatPer * 100000
AER6$ACT100K <- AER6$ACTPer * 100000
AER6$NSW100K <- AER6$NSWPer * 100000
AER6$QLD100K <- AER6$QLDPer * 100000
AER6$SA100K <- AER6$SAPer * 100000
AER6$TAS100K <- AER6$TASPer * 100000
AER6 <- AER6 %>% relocate(quarter)

# AER7
AER07.7 <- AER07 %>%
  filter(National %in% c("Retailer", "National Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))
AER07.7 <- AER07.7 %>%
  select(-c(2, 3, 4, 5, 6))
AER7 <- as.data.frame(t(AER07.7), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER7) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER7 <- AER7 %>%
  filter(!row_number() %in% 1)
AER7$quarter <- seq(as.yearqtr("2020 Q4"), as.yearqtr("2021 Q4"), by = 1/4)
AER7$NatPer <- as.numeric(AER7$NatPer)
AER7$ACTPer <- as.numeric(AER7$ACTPer)
AER7$NSWPer <- as.numeric(AER7$NSWPer)
AER7$QLDPer <- as.numeric(AER7$QLDPer)
AER7$SAPer <- as.numeric(AER7$SAPer)
AER7$TASPer <- as.numeric(AER7$TASPer)
AER7$NAT100K <- AER7$NatPer * 100000
AER7$ACT100K <- AER7$ACTPer * 100000
AER7$NSW100K <- AER7$NSWPer * 100000
AER7$QLD100K <- AER7$QLDPer * 100000
AER7$SA100K <- AER7$SAPer * 100000
AER7$TAS100K <- AER7$TASPer * 100000
AER7 <- AER7 %>% relocate(quarter)

# AER8
AER08.8 <- AER08 %>%
  filter(National %in% c("Retailer", "National Total", "ACT Total", "NSW Total", "QLD Total",
                         "SA Total", "TAS Total"))
AER08.8 <- AER08.8 %>%
  select(-c(2, 3, 4, 5, 6))
AER8 <- as.data.frame(t(AER08.8), row.names = c(1,2,3,4,5,6,7,8))
colnames(AER8) <- c("date", "NatPer", "ACTPer", "NSWPer", "QLDPer", "SAPer", "TASPer")
AER8 <- AER8 %>%
  filter(!row_number() %in% 1)
AER8$quarter <- seq(as.yearqtr("2022 Q1"), as.yearqtr("2023 Q1"), by = 1/4)
AER8$NatPer <- as.numeric(AER8$NatPer)
AER8$ACTPer <- as.numeric(AER8$ACTPer)
AER8$NSWPer <- as.numeric(AER8$NSWPer)
AER8$QLDPer <- as.numeric(AER8$QLDPer)
AER8$SAPer <- as.numeric(AER8$SAPer)
AER8$TASPer <- as.numeric(AER8$TASPer)
AER8$NAT100K <- AER8$NatPer * 100000
AER8$ACT100K <- AER8$ACTPer * 100000
AER8$NSW100K <- AER8$NSWPer * 100000
AER8$QLD100K <- AER8$QLDPer * 100000
AER8$SA100K <- AER8$SAPer * 100000
AER8$TAS100K <- AER8$TASPer * 100000
AER8 <- AER8 %>% relocate(quarter)


# join all AER DFs, sort by quarter and remove duplicates
AER <- AER1 %>%
  full_join(AER2) %>%
  full_join(AER3) %>%
  full_join(AER4) %>%
  full_join(AER5) %>%
  full_join(AER6) %>%
  full_join(AER7) %>%
  full_join(AER8) %>%
  arrange(desc(-quarter)) %>%
  distinct(quarter, .keep_all = TRUE)


# joining all DFs to return a poverty DF
p.df <- AER %>%
  full_join(cpi_nsw, by = "quarter") %>%
  full_join(epi_nsw, by = "quarter") %>%
  full_join(covid, by = "quarter")

# returning a lag(quarter = 1) dependent variable in the poverty DF
p.df$lag.NSW100K <- lag(p.df$NSW100K, n = 1)

# linear model using OLS
p.lm <- lm(NSW100K ~ CPI_NSW + EPI_NSW + covid, data = p.df)
summary(p.lm)
bptest(p.lm)
anova(p.lm)

# linear model with a lagged dependent variable (lag = one quarter)
p.lag.lm <- lm(lag.NSW100K ~ CPI_NSW + EPI_NSW + covid, data = p.df)
summary(p.lag.lm)
bptest(p.lag.lm)

# log-log linear model with a lagged dependent variable
p.log.lm <- lm(log(lag.NSW100K) ~ log(CPI_NSW) + log(EPI_NSW) + covid, data = p.df)
summary(p.log.lm)
bptest(p.log.lm)

# linear model using a Huber MM robust estimator
p.lm.robust <- MASS::rlm(NSW100K ~ CPI_NSW + EPI_NSW + covid, data = p.df, method = "MM")
summary(p.lm.robust)
bptest(p.lm.robust)

# weighted least squares model
wt <- (1 / lm(abs(p.lm$residuals) ~ p.lm$fitted.values)$fitted.values^2)

p.wls <- lm(NSW100K ~ CPI_NSW + EPI_NSW + covid, data = p.df, weights = wt)
summary(p.wls)
bptest(p.wls)



# plotting residuals versus fitted values for each model

p.df <- p.df %>% add_residuals(p.lm)
# p.df <- p.df %>% add_residuals(p.lag.lm)
# p.df <- p.df %>% add_residuals(p.log.lm)
# p.df <- p.df %>% add_residuals(p.lm.robust)

p.df %>%
  ggplot(aes(quarter, resid)) + geom_ref_line(h=0) + geom_line(colour = "grey50") +
  geom_smooth(se = TRUE, span = 0.20) +
  theme(axis.text.x = element_text(size = 8, angle = 45)) +
  scale_x_yearqtr(breaks = as.yearqtr(p.df$quarter), labels = format(p.df$quarter,"%Y Q%q"), n = 31)


ggplot(p.lm, aes(x=fitted(p.lm), 
    y=residuals(p.lm))) + 
  geom_point() + geom_smooth() + xlab("Fitted Values") + 
  ylab("Residuals")






