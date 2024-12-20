#################################################################################
## Visualise and collect the mastercard data
#################################################################################

# Functionality shown in this vignette
# - load mastercard data (parquet files)
# - collect initially (non-DP) time series

# Clean the workspace and console
closeAllConnections(); rm(list=ls()); cat("\014")
graphics.off(); start_time = as.numeric(Sys.time())

# Set working directory to source
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir); folres = paste0("./results/")

library(plotly)
library(dplyr)

# Main mastercard data in parts
data = read.csv("pets_challenge_in_person.csv")
head(data)
# Convert to proper date and remove NAs
data$date = as.Date(data$date)
data = na.omit(data)

# Data details
len = length(data$date)
tdate = unique(data$date)
merch = unique(data$merch_category)
nmerch = length(merch)

# Find NAs among dates
idna = which(is.na(data$date))
if(length(idna) > 0){
  paste("Some NAs in data")
}

# Limit data to time period of interest
tend = "2021-01-23"
tstart = "2020-01-18"
tval = tdate[tdate <= tend & tdate >= tstart]
lval = length(tval)

# For every date obtain sums of transactions and amounts
totSpend = rep(0, lval); totTrans = totSpend
# Also separate by each merchant category
merchSpend = matrix(NA, nmerch, lval); merchTrans = merchSpend
for (i in 1:lval) {
  id = which(data$date == tval[i])
  # Total transactions
  totSpend[i] = sum(data$spendamt[id])
  totTrans[i] = sum(data$nb_transactions[id])
  # Separation by merchant category
  for (j in 1:nmerch) {
    idmerch = which(data$merch_category[id] == merch[j])
    merchSpend[j, i] = sum(data$spendamt[id[idmerch]])
    merchTrans[j, i] = sum(data$nb_transactions[id[idmerch]])
  }
}

# Plot against dates
tdates = as.Date(tval)

# Get total spend and no.transactions as data frames
dfSpend = data.frame(tdates, totSpend)
dfTrans = data.frame(tdates, totTrans)
# Merchant level data frames
dfmerchSpend = data.frame(tdates, t(merchSpend))
dfmerchTrans = data.frame(tdates, t(merchTrans))

# Order the data frames by ending date
dfSpend = dfSpend %>% arrange(tdates)
dfTrans = dfTrans %>% arrange(tdates)
dfmerchSpend = dfmerchSpend %>% arrange(tdates)
dfmerchTrans = dfmerchTrans %>% arrange(tdates)

# Write overall time series as csv files
nam1 = paste0(c(folres, "totSpend"), collapse = '')
nam2 = paste0(c(folres, "totTrans"), collapse = '')
write.table(dfSpend, file = nam1, row.names=FALSE, col.names=FALSE)
write.table(dfTrans, file = nam2, row.names=FALSE, col.names=FALSE)

# Write the merchant level time series as csv files
nam1 = paste0(c(folres, "merchSpend"), collapse = '')
nam2 = paste0(c(folres, "merchTrans"), collapse = '')
nam3 = paste0(c(folres, "merchTypes"), collapse = '')
write.table(dfmerchSpend, file = nam1, row.names=FALSE, col.names=FALSE)
write.table(dfmerchTrans, file = nam2, row.names=FALSE, col.names=FALSE)
write.table(merch, file = nam3, row.names=FALSE, col.names=FALSE)

# Totals across all merchants
fig <- plot_ly()
# Add traces
fig <- fig %>% add_trace(x = dfSpend$tdates, y = dfSpend$totSpend, name = "amount", mode = "lines+markers", type = "scatter")

ay <- list(
  overlaying = "y",
  side = "right",
  title = "total transactions")

fig <- fig %>% add_trace(x = dfTrans$tdates, y = dfTrans$totTrans, name = "number", yaxis = "y2", mode = "lines+markers", type = "scatter")

# Set figure title, x and y-axes titles
fig <- fig %>% layout(
  yaxis2 = ay,
  xaxis = list(title='time (days)'),
  yaxis = list(title="total spend")
)%>%
  layout(plot_bgcolor='#e5ecf6',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 2,
           gridcolor = 'ffff')
  ); fig


#################################################################################
## Construct DP version of data from python notebook
#################################################################################

# Python file performing DP
script = '../python/dp_mastercard.py'
dataset_path = file.path(this.dir, 'pets_challenge_in_person.csv')

# Add desired epsilon value for DP
epsilon_value = 1.0

# Perform the DP computations and save csvs
command <- sprintf("python3 %s %s %f", script, dataset_path, epsilon_value)
system(command)
