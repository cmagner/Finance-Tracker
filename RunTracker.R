RunTracker <- function() {

    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
    
    setwd("~/Bills, Finances, Taxes & Legal/FinanceTracker")

    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/ParseQuickenDump.R')
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/AddMajorCategories.R')
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/plotCashBurnDown.R')

    t1 <- ParseQuickenDump("QuickenDump_05162015.TXT")
    t2 <- AddMajorCategories(t1,"Categories.csv")
    cash <-plotCashBurnDown(t2)
}
