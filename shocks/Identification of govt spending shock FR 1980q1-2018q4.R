###########################################################
# Identify government spending shock through VAR(4) model #
###########################################################

# France, 1980q1-2018q4

# Import French fiscal data and other data
library(readxl)
fFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_Fiscal_Database_July2019.xlsx",
  sheet = "FR")
oFR <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
  sheet = "FR", n_max = 157)

# Rename first column of both datasets to 'quarter'
library(dplyr)
fFR1 <- fFR %>% 
  rename(quarter = ...1,)
oFR1 <- oFR %>% 
  rename(quarter = ...1,)

# Add the 10-year interest rate to the fiscal dataset
fFR2 <- fFR1 %>%
  mutate(intFR = oFR1$R)

# Create variables for real government spending & net tax revenues, and take logs
fFR3 <- fFR2 %>%
  mutate(trFR = TOR - (THN + SIN), gFR = GCN + GIN) %>%
  mutate(rtrFR = trFR/(P/100), rgFR = gFR/(P/100), ryFR = Y/(P/100)) %>%
  mutate(lrtrFR = log(rtrFR), lrgFR = log(rgFR), lryFR = log(ryFR), lPFR = log(P))

# Create time series
library(zoo)
names(fFR3)[1] <- "quarterFR"
fFR4 <- fFR3 %>%
  mutate(quartersFR = as.yearqtr(quarterFR))   ## declare that the "quarter" column contains dates.
fFR5 <- subset(fFR4, select = -c(quarterFR))
fFR6 <- fFR5[, c(24, 1:23)]
fFRts <- ts(fFR6, start=c(1980, 1), end=c(2018, 4), frequency=4)   ## create time series
View(fFRts)

# Create log variables
lrtrFR <- fFRts[, "lrtrFR"]
lrgFR <- fFRts[, "lrgFR"]
lryFR <- fFRts[, "lryFR"]
lPFR <- fFRts[, "lPFR"]
intFR <- fFRts[, "intFR"]

# KPSS tests for level and trend stationarity
library(tseries)
kpss.test(lrtrFR)
kpss.test(lrtrFR, null = "T")
kpss.test(lrgFR)
kpss.test(lrgFR, null = "T")
kpss.test(lryFR)
kpss.test(lryFR, null = "T")
kpss.test(lPFR)
kpss.test(lPFR, null = "T")
kpss.test(intFR)
kpss.test(intFR, null = "T")

# Create dataframe and time series for VAR model
d.lFR <- data.frame(cbind(lrtrFR, lrgFR, lryFR, lPFR, intFR))
d.lFRts <- ts(d.lFR, start=c(1980, 1), end=c(2018, 4), frequency=4) 

# Include dummy variables for 1992q1-1995q4 (see Alloza et al., page 4, footnote 10)
quartersFR <- fFRts[, "quartersFR"]
dumFR <- as.numeric(quartersFR >= "1992 Q1" & quartersFR <= "1995 Q4")
dumFR[49] <- 1
dumFR[62:64] <- 1
dumFRts <- ts(dumFR, start=c(1980, 1), end=c(2018, 4), frequency=4) 
q <- time(d.lFRts)
ex <- cbind(I(q)^2, dumFRts)

# Optimal lag length
library(vars)
VARselect(d.lFRts, lag.max = 6, type = "both", exogen = ex)   

# VAR model, which contains four lags, a constant, a trend and a dummy (see Alloza et al.)
varFR1 <- VAR(d.lFRts, p = 4, type = "both", exogen = ex)
summary(varFR1)

# Eigenvalues: stability of VAR process (stable if values < 1)
roots(varFR1, modulus = TRUE)
# Test for serially correlated errors
serial.test(varFR1, lags.pt = 16)
# Normality, multivariate skewness and kurtosis test
normality.test(varFR1)

# Identify the structural government spending (lrg) shock
resFR1 <- residuals(varFR1)
resFR2 <- ts(resFR1, start=c(1980, 1), end=c(2018, 4), frequency=4) 
# Obtain the residuals of lrg and lP - see Alloza et al. (2019)
res.lrgFR <- subset(resFR2, TRUE, lrgFR, drop = FALSE)
# Compute structural d.lrg shock - see Alloza et al. (2019), page 4-5, Eq. 3 and footnote 11
shock.lrgFR <- res.lrgFR
shock.lrgFR

# Add structural shock vector to time series
fFRts1 <- cbind(fFRts, shock.lrgFR)
colnames(fFRts1) <- c("quartersFR", "TOR", "DTX", "SCT", "TIN", "TOE", "THN", "GCN", "COE", "SIN",
                      "GIN", "INP", "Y", "P", "intFR", "trFR", "gFR", "rtrFR", "rgFR", "ryFR", 
                      "lrtrFR", "lrgFR", "lryFR", "lPFR", "shock_gFR")
