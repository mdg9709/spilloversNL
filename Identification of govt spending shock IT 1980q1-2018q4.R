###########################################################
# Identify government spending shock through VAR(4) model #
###########################################################

# Italy, 1980q1-2018q4

# Import Italian fiscal data and other data
library(readxl)
fIT <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_Fiscal_Database_July2019.xlsx",
  sheet = "IT")
oIT <- read_excel("~/Studie/MSc ECO/Period 5-6 MSc thesis/Data/Main datasets/ABP_other_variables.xlsx", 
  sheet = "IT", n_max = 157)

# Rename first column of both datasets to 'quarter'
library(dplyr)
fIT1 <- fIT %>% 
  rename(quarterIT = ...1,)
oIT1 <- oIT %>% 
  rename(quarterIT = ...1,)

# Add the 10-year interest rate to the fiscal dataset
fIT2 <- fIT1 %>%
  mutate(intIT = oIT1$R)

# Create variables for real government spending & net tax revenues, and take logs
fIT3 <- fIT2 %>%
  mutate(trIT = TOR - (THN + SIN), gIT = GCN + GIN) %>%
  mutate(rtrIT = trIT/(P/100), rgIT = gIT/(P/100), ryIT = Y/(P/100)) %>%
  mutate(lrtrIT = log(rtrIT), lrgIT = log(rgIT), lryIT = log(ryIT), lPIT = log(P))

# Create time series
library(zoo)
names(fIT3)[1] <- "quarterIT"
fIT4 <- fIT3 %>%
  mutate(quartersIT = as.yearqtr(quarterIT))   # declare that the "quarter" column contains dates.
fIT5 <- subset(fIT4, select = -c(quarterIT))
fIT6 <- fIT5[, c(24, 1:23)]
fITts <- ts(fIT6, start=c(1980, 1), end=c(2018, 4), frequency=4)   # create time series
View(fITts)

# Create log variables
lrtrIT <- fITts[, "lrtrIT"]
lrgIT <- fITts[, "lrgIT"]
lryIT <- fITts[, "lryIT"]
lPIT <- fITts[, "lPIT"]
intIT <- fITts[, "intIT"]

# KPSS tests for level and trend stationarity
library(tseries)
kpss.test(lrtrIT)
kpss.test(lrtrIT, null = "T")
kpss.test(lrgIT)
kpss.test(lrgIT, null = "T")
kpss.test(lryIT)
kpss.test(lryIT, null = "T")
kpss.test(lPIT)
kpss.test(lPIT, null = "T")
kpss.test(intIT)
kpss.test(intIT, null = "T")

# Create dataframe and time series for VAR model
d.lIT <- data.frame(cbind(lrtrIT, lrgIT, lryIT, lPIT, intIT))
ts.IT <- ts(d.lIT, start=c(1980, 1), end=c(2018, 4), frequency=4)   # create time series
q <- time(ts.IT)
ex <- I(q)^2

# Optimal lag length
library(vars)
VARselect(ts.IT, lag.max = 6, type = "both", exogen = ex)

# VAR model, which contains four lags, a constant and a trend (see Alloza et al.)
varIT1 <- VAR(ts.IT, p = 4, type = "both", exogen = ex)
summary(varIT1)

# Eigenvalues: stability of VAR process (stable if values < 1)
roots(varIT1, modulus = TRUE)
# Test for seriallYIT correlated errors
serial.test(varIT1, lags.pt = 16)
# Normality, multivariate skewness and kurtosis test
normality.test(varIT1)

# Identify the structural government spending (lrg) shock
resIT1 <- residuals(varIT1)
resIT2 <- ts(resIT1, start=c(1980, 1), end=c(2018, 4), frequency=4) 
# Obtain the residuals of lrg and lP - see Alloza et al. (2019)
res.lrgIT <- subset(resIT2, TRUE, lrgIT, drop = FALSE)
res.lPIT <- subset(resIT2, TRUE, lPIT, drop = FALSE)
# Compute structural d.lrg shock - see Alloza et al. (2019), page 4-5, Eq. 3 and footnote 11
shock.lrgIT <- res.lrgIT - (-0.5)*res.lPIT
shock.lrgIT

# Add structural shock vector to time series
fITts1 <- cbind(fITts, shock.lrgIT)
colnames(fITts1) <- c("quartersIT", "TOR", "DTX", "SCT", "TIN", "TOE", "THN", "GCN", "COE", "SIN",
                      "GIN", "INP", "Y", "P", "intIT", "trIT", "gIT", "rtrIT", "rgIT", "ryIT", 
                      "lrtrIT", "lrgIT", "lryIT", "lPIT", "shock_gIT")
