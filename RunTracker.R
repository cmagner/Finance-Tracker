RunTracker <- function(filename) {

    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
    
    setwd("~/Bills, Finances, Taxes & Legal/FinanceTracker")

    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/ParseQuickenDump.R')
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/AddMajorCategories.R')
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/plotCashBurnDown.R')

    t1 <- ParseQuickenDump(filename)
    t2 <- AddMajorCategories(t1,"Categories.csv")
    cash <-plotCashBurnDown(t2)
}
