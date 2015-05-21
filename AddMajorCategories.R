AddMajorCategories <- function( trans_tbl, cat_filename ) {
    
    library(dplyr)
    library(tidyr)
    library(lubridate)

    setwd("~/Bills, Finances, Taxes & Legal/FinanceTracker")
    source('~/Bills, Finances, Taxes & Legal/FinanceTracker/AddMajorCategories.R')
    
    cats <- tbl_df( read.csv(cat_filename, header=TRUE, strip.white=TRUE, colClasses="character") )
    
    # Warn of any transactions with Category values not defined in the 
    # Categories table file.
    noDefCat <- 
        trans_tbl %>%
        filter( Type != "TRANSFER", 
                Category = !is.element(Category, cats$SubCategory) )    
    if (nrow(noDefCat) > 0) {
        warning("Found transactions with unrecognized categories:")
        print(noDefCat)
    }

    # Add column to the transaction table for Majory.Category (extracted from 
    #   the Categories.CSV file)
    # Reorder columns
    trans_tbl <- mutate(trans_tbl, Major.Category=NA )
    for (n in 1:nrow(trans_tbl)) {
        trans_tbl$Major.Category[n] <- as.character(
            cats %>% 
            filter( SubCategory == trans_tbl$Category[n] ) %>%
            select( Category ) )
    }
    return 
        trans_tbl %>%
        select( Date, Week, Month, Account.Type, Account.Name, Type, 
                Major.Category, Category, Amount, Description, Memo )
    
    
}
