###########################################################
# Identify government spending shock through VAR(4) model #
###########################################################

# Spain, 1980q1-2018q4

# Import Spanish fiscal data and other data
library(readxl)
fES <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_Fiscal_Database_July2019.xlsx",
  sheet = "ES")
oES <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
  sheet = "ES", n_max = 157)

# Rename first column of both datasets to 'quarter'
library(dplyr)
fES1 <- fES %>% 
  rename(quarter = ...1,)
oES1 <- oES %>% 
  rename(quarter = ...1,)

# Add the 10-year interest rate to the fiscal dataset
fES2 <- fES1 %>%
  mutate(intES = oES1$R)

# Create variables for real government spending & net tax revenues, and take logs
fES3 <- fES2 %>%
  mutate(trES = TOR - (THN + SIN), gES = GCN + GIN) %>%
  mutate(rtrES = trES/(P/100), rgES = gES/(P/100), ryES = Y/(P/100)) %>%
  mutate(lrtrES = log(rtrES), lrgES = log(rgES), lryES = log(ryES), lPES = log(P))

# Create time series
library(zoo)
names(fES3)[1] <- "quarterES"
quarterES <- fES3$quarterES
fES4 <- fES3 %>%
  mutate(quartersES = as.yearqtr(quarterES))   ## declare that the "quarter" column contains dates.
fES5 <- subset(fES4, select = -c(quarterES))
fES6 <- fES5[, c(24, 1:23)]
fESts <- ts(fES6, start=c(1980, 1), end=c(2018, 4), frequency=4)   ## create time series
View(fESts)

# Create the log variables
lrtrES <- fESts[, "lrtrES"]
lrgES <- fESts[, "lrgES"]
lryES <- fESts[, "lryES"]
lPES <- fESts[, "lPES"]
intES <- fESts[, "intES"]

# KPSS tests for level and trend stationarity
library(tseries)
kpss.test(lrtrES)
kpss.test(lrtrES, null = "T")
kpss.test(lrgES)
kpss.test(lrgES, null = "T")
kpss.test(lryES)
kpss.test(lryES, null = "T")
kpss.test(lPES)
kpss.test(lPES, null = "T")
kpss.test(intES)
kpss.test(intES, null = "T")

# Create dataframe and time series for VAR model
d.lES <- data.frame(cbind(lrtrES, lrgES, lryES, lPES, intES))
d.lESts <- ts(d.lES, start=c(1980, 1), end=c(2018, 4), frequency=4)
q <- time(d.lESts)
ex <- I(q)^2

# Optimal lag length
library(vars)
VARselect(d.lESts, lag.max = 6, type = "both", exogen = ex)   

# VAR model, which contains four lags, a constant and a trend (see Alloza et al.)
varES1 <- VAR(d.lESts, p = 4, type = "both", exogen = ex)
summary(varES1)

# Eigenvalues: stability of VAR process (stable if values < 1)
roots(varES1, modulus = TRUE)
# Test for serially correlated errors
serial.test(varES1, lags.pt = 16) 
# Normality, multivariate skewness and kurtosis test
normality.test(varES1)

# Identify the structural government spending (lrg) shock
resES1 <- residuals(varES1)
resES2 <- ts(resES1, start=c(1980, 1), end=c(2018, 4), frequency=4) 
# Obtain the residuals of lrg and lP - see Alloza et al. (2019)
res.lrgES <- subset(resES2, TRUE, lrgES, drop = FALSE)
res.lPES <- subset(resES2, TRUE, lPES, drop = FALSE)
# Compute structural lrg shock - see Alloza et al. (2019), page 4-5, Eq. 3 and footnote 11
shock.lrgES <- res.lrgES - (-0.5)*res.lPES
shock.lrgES

# Add structural shock vector to time series
fESts1 <- cbind(fESts, shock.lrgES)
colnames(fESts1) <- c("quarter", "TOR", "DTX", "SCT", "TIN", "TOE", "THN", "GCN", "COE", "SIN", 
                      "GIN", "INP", "Y", "P", "intES", "trES", "gES", "rtrES", "rgES", "ryES", 
                      "lrtrES", "lrgES", "lryES", "lPES", "shockES") 
