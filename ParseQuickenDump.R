ParseQuickenDump <- function(filename) {
    
    library(dplyr)
    library(tidyr)
    library(lubridate)

    setwd("~/Bills, Finances, Taxes & Legal/FinanceTracker")
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/ParseQuickenDump.R')
    
    # Read in the tab-delimited Quicken dump file
    d <- read.delim(filename, header=FALSE, skip=6, colClasses="character")
    
    # Drop bogus first and last columns, and convert to a dplyr tbl
    # Set column names
    rawData <- tbl_df( d[,2:7] )
    names(rawData) <- c("Date","Account","Num","Description","Memo","Amount")
    rm("d")
    
    # Remove blank lines
    # Add column for row number and category
    rawData <- filter( rawData, Date != "" )
    rawData <- mutate( rawData, Row = 1:nrow(rawData), Category = NA )       
    
    # Separate the actual transactions from the rest of the observations. 
    # Convert Date values to actual date type.
    # Convert the Amount values to actual numbers. 
    # Add a column for Type (INCOME,EXPENSE,TRANSFER).
    # Rename account type, "Fulton - Business Checking"
    trans <- 
        rawData %>%
        filter( !is.na(mdy(Date)) ) %>%
        mutate( Date = mdy(Date)) %>%
        mutate( Amount = as.numeric(gsub(",", "", Amount)) ) %>%
        mutate( Type = NA )
    trans$Type[trans$Amount>=0] <- "INCOME"
    trans$Type[trans$Amount<0] <- "EXPENSE"
    trans$Account[trans$Account == "Fulton - Business Checking"] <- "Checking - Fulton Bank Business"
        
    # Extract the categories from the raw data.  Massage into a table of 
    # categories and their corresponding range of transactions.  Discard any 
    # unused categories.
    cats <-
        rawData %>%
        filter( is.na(mdy(Date)) ) %>%
        select( Date, Row ) %>% 
        rename( Category = Date, RowLO = Row ) %>% 
        mutate( RowHI = NA )
    cats$RowHI[-nrow(cats)] <- cats$RowLO[-1]
    cats <- cats[-nrow(cats),]
    cats <- 
        cats %>%
        filter( (RowHI - RowLO) > 1 ) %>%
        mutate( RowLO = RowLO+1, RowHI = RowHI-1 )
    
    # Set the category associated with each transaction
    for (n in 1:nrow(cats)) {   
        cat <- cats[n,]
        rowsThisCat <- (trans$Row >= cat$RowLO) & (trans$Row <= cat$RowHI)
        trans[rowsThisCat,]$Category <- cat$Category
    }

    # Set Type to TRANSFER for applicable transactions
    # Discard any negative TRANSFER transactions
    # Discard any transactions with Amount values equal to $0.00.
    # Drop the sign on all Amount values 
    trans$Type[grep("[[]", trans$Category)] <- "TRANSFER"
    trans <-
        trans %>%
        filter( !(Type == "TRANSFER" & Amount < 0.00) ) %>%
        filter( Amount != 0.00 ) %>%
        mutate( Amount = abs(Amount) )
    
    # Make transfers to the HELOC account, expenses with category 
    # "Home Equity Line of Credit"
    transHELOC <- trans$Account == "HELOC - Fulton Bank"
    trans$Type[transHELOC] <- "EXPENSE"
    trans$Account[transHELOC] <- 
        substr( trans$Category[transHELOC], 2, nchar(trans$Category[transHELOC])-1 )
    trans$Category[transHELOC] <- "Home Equity Line of Credit"
    
    # Show any HSA contributions as a transfer rather than expense
    transHSA <- trans$Category == "Medical:HSA Savings"
    trans$Type[transHSA] <- "TRANSFER"
    trans$Category[transHSA] <- paste("[", trans$Account[transHSA], "]", sep="")
    trans$Account[transHSA] <- "HSA - Highmark Blueshield"
    
    # Add columns for week number and month number
    # split the Account column into account type and name
    # Return reordered table columns sorted by Date, then Type, then Amount
    trans %>%
        mutate( Week=week(Date), Month=month(Date) ) %>%
        separate( col=Account, into=c("Account.Type","Account.Name"), sep=" - ") %>%
        select( Date, Week, Month, Account.Type, Account.Name, Type, Category, Amount, Description, Memo ) %>%
        arrange( Date, Type, Amount )
}
