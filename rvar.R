# Load the vars library to be able to use
# VAR() and the predict() function for
# VAR models
library("vars")

# Some properties of the model have to
# be predefined.
# model.lag.max: the maximum lag between
#   all steps.
# t.threshold: threshhold value for the
#   t-statistics in the iterative exlusion
#   of uncorrelated variables.
model.lag.max <- 30
t.threshold   <- 2

# Data is read from a CSV file.
# The data consists of daily aggregates of
# quantities per process step. In this
# example there are three process steps:
# SO: sales order
# GS: goods shipped
# IS: invoice sent
data.raw <- read.csv( file="Data/Sales-NL01-Quantities.csv", sep=";", header=TRUE, colClasses=c('Date', 'numeric', 'numeric', 'numeric') )

# Missing dates in the provided CSV files
# are filled by merging an empty data frame
# containing all dates with the provided
# data. Missing dates between the first and
# last day of the provided data is filled
# with zeros.
data.empty <- data.frame( Date=seq.Date( from=as.Date( head( sort( data.raw[,1] ), 1 ) ), to=as.Date( tail( sort(data.raw[,1]), 1) ), by="1 day") )
data.merged <- merge( data.empty, data.raw, by = c("Date"), all.x=TRUE, all.y=FALSE )
data.merged[ is.na(data.merged) ] <- 0

# A multipe time series object is created
# by using the ts function on the merged
# data frame. This mts object can be used
# by the vars package for modeling.
data.tseries <- ts( data = data.merged[, 2:4] )

# The VAR is modeled by using the VAR
# function from the vars package based on
# the mts object.
# A maximum lag can be provided and since
# trend and constant terms should not be
# included in the model, type is set to none.
# In this case the model contains all
# of the variables restricted by lag.max
# (30) in this example.
model.var <- VAR( data.tseries, p=1, lag.max=30, type="none" )

# The VAR model is restricted further
# to exclude all weakly correlated
# variables from the model.
model.var.restricted <- restrict( model.var, thresh=2, method = "ser" )

# Serveral built-in functions can be used
# to present the resulting restricted model.
summary( model.var.restricted )
plot( model.var.restricted )

# The predict function from the vars package
# can be used to predict a number of dates
# following the provided data. In this
# example 10 days are predicted within a
# 95% confidence interval based on the
# restricted model.
model.predictions <- predict( model.var.restricted, n.ahead = 10, ci = 0.01)
print(model.predictions$fcst$SO[,1])

# Serval built-in functions can be used
# to present the resulting predictions.
fanchart( model.predictions )
plot( model.predictions )
