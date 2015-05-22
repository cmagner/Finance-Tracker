plotCashBurnDown <- function( trans ) {

    dollar_str <- function(num, cents=FALSE) {
        val <- if (cents) round(num,2) else round(num,0)
        negs <- (val < 0)
        str <- gsub(" ", "", 
            paste("$", format( abs(val), big.mark=",", big.interval=3 ), sep=""))
        str[negs] <- paste("-", str[negs], sep="")        
    }
    
    
    # ------------------------------------------------------------------------
    # VARIOUS MAGIC NUMBERS
    #
    #     Starting Bank Balances (as of 3/15):    
    #       Clearview               $5,600 
    #       Fulton - personal        $2,867 
    #       Fulton - business   	$563 
    #                       Total	$9,030 
    #     
    #     TruePostition Lay-Off Money:	
    #       Net Vacation Pay        $2,036 
    #       Net Severance Pkg	    $63,442 
    #
    startCashInBank <- 5600 + 2867 + 563
    cashFromTP <- 2036 + 63442
    #
    #     Estimated Extraordinary Expenditures (known):    
    #       Hawaii Vacation	        $10,000 
    #       Retaining Wall	        $2,500 
    #       New Couch	            $1,000 
    #
    estExpenses <- tbl_df( data.frame(
        Expense = c("Hawaii Vacation", "Retaining Wall", "New Couch"),
        Amount = c(10000, 2500, 1000),
        DateIncurred = c(mdy("3/24/2015"), NA, NA)) )
    #
    #     Presumed Income Rates (per week):	
    #       Trueposition Net Salary $1,628 
    #       Magner Com Gross Inc.   $473 
    #     
    #     Paid for from TruePosition Gross (per week):	
    #       Health Insurance	    $360 
    #       HSA Savings	            $113 
    #
    estBurnRateWk <- sum(c(1628, 473, 360, 113))
    
    # ------------------------------------------------------------------------
    # Adjust the week numbers so that week starts on Sunday
    trans <- mutate(trans, Week = week(trans$Date - 4*24*60*60) %% 52)
    startWeek <- min(trans$Week)
    latestWeek <- max(trans$Week)
    currentWeek <- week(now() - 4*24*60*60) %% 52
    currentWeekFrac <- as.numeric(now() - mdy("1/1/2015") - 4) / 7
    todayFrcWk <- as.numeric(now() - mdy("1/1/2015")) / 7
  
#     latestDate <- max(trans$Date)  
#     currentDate <- now()
    
    # ------------------------------------------------------------------------
    # Create a table containing the weekly income and expense totals for every
    # week to date
    allWks <- data.frame( Week=min(trans$Week) : max(trans$Week[trans$Type != "TRANSFER"]) )
    exp <- 
        trans %>% 
        filter(Type=="EXPENSE") %>% 
        group_by(Week) %>% 
        summarize( Expense=sum(Amount) )
    inc <- 
        trans %>% 
        filter(Type=="INCOME") %>% 
        group_by(Week) %>% 
        summarize( Income=sum(Amount) )    
    tblIncExp <- 
        left_join( left_join( allWks, inc, by="Week" ), exp, by="Week")
    tblIncExp[is.na(tblIncExp)] = 0.00
    
    # ------------------------------------------------------------------------
    # Initialize a table for actual and predicted cash remaining as a function
    # of week
    cashLeft <- tbl_df(data.frame(Week = startWeek:week(mdy("12/31/2015"))))
    
    # ------------------------------------------------------------------------
    # Create the actual cash remaining as a function of week
    cashLeft$Actual <- NA
    cashLeft$Actual[1] <- startCashInBank
    for (n in 1:nrow(tblIncExp)) {
        cashLeft$Actual[n+1] <- 
            cashLeft$Actual[n] + tblIncExp$Income[n] - tblIncExp$Expense[n]        
    }
    
    # ------------------------------------------------------------------------
    # Create the predicated cash remaining as a function of week
    cashLeft$Predicted[1] <- startCashInBank
    rowIdx <- nrow(tblIncExp) + 1
    for (n in 1:rowIdx) {
        cashLeft$Predicted[n+1] <- cashLeft$Actual[n] - estBurnRateWk
    }
    rowIdx <- rowIdx + 1
    cashLeft$Predicted[rowIdx] <- 
        cashLeft$Predicted[rowIdx] - sum(estExpenses$Amount[is.na(estExpenses$DateIncurred)])
    for (n in rowIdx:(nrow(cashLeft)-1)) {
        cashLeft$Predicted[n+1] <- cashLeft$Predicted[n] - estBurnRateWk
    }

    # ------------------------------------------------------------------------
    # Find vertical line which Predicted Cash intercepts the x-asis (i.e. the
    # predicted Burn Down zero crossing )
    fit<-coef(lm(cashLeft$Predicted[rowIdx:nrow(cashLeft)] 
               ~ cashLeft$Week[rowIdx:nrow(cashLeft)]))
    zeroCashWeek <- -fit[1]/fit[2]
    zeroCashDate <- mdy("1/1/2015")
    second(zeroCashDate) <- ((zeroCashWeek*7) + 4)*24*60*60

    # ------------------------------------------------------------------------
    cashLeft <- mutate(cashLeft,partialWeek=Week >= currentWeek)
    # Plot the estimated, actual and predicated cash remaining
    p <- ggplot(cashLeft, aes(x=Week)) +
        geom_area(fill="white", colour="honeydew3", alpha=0.5, aes(x=Week,y=Predicted), show_guide=TRUE) +
        geom_bar(colour="black", stat="identity", alpha=0.25, aes(y=Actual, fill=!partialWeek)) +
        labs(
            title=paste("Cash Burn Down - ",month(today(),label=TRUE)," ",day(today()),", ",year(today()),sep=""), 
                        x="Week/Month", y="Remaining Cash") +
    geom_vline(
        xintercept = as.numeric(mdy(paste(4:11,"/1/2015")) - mdy("1/1/2015"))/7, 
        linetype=3, colour="wheat4") +
    annotate("text", y = -1.5e4, size=rel(8), colour="steelblue4",
             x = as.numeric(mdy(paste(4:10,"/1/2015")) - mdy("1/1/2015"))/7 +
                 as.numeric(mdy(paste(5:11,"/1/2015")) - mdy(paste(4:10,"/1/2015")))/14,
             label = month(4:10,label=TRUE)) +
    annotate("segment", x=todayFrcWk, xend=todayFrcWk, 
             y=-5e3, yend=6e4, colour="firebrick", linetype=3, size=rel(1)) +
    annotate("segment", x=zeroCashWeek,  xend=zeroCashWeek, 
             y=-5e3, yend=6e4, colour="firebrick", linetype=3, size=rel(1)) +
    annotate("text", y = -8e3, x = todayFrcWk, size=6, colour="firebrick4", face="bold", 
             label = "today", sep="/") +
    annotate("text", y = -8e3, x = zeroCashWeek, size=6, colour="firebrick4", face="bold",
             label = paste( month(zeroCashDate,label=TRUE), day(zeroCashDate)) ) +
    theme(
        plot.title = element_text(size=rel(2.5), face="bold" ),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype=3, colour="wheat4"),
        panel.grid.minor.y = element_line(linetype=3, colour="wheat4"),
        axis.title = element_text(size=rel(2), face="bold"), 
        axis.text = element_text(size=rel(1.5), colour="steelblue4") ) +
    scale_y_continuous(labels = dollar) +
    coord_cartesian(xlim = c(10,45), ylim = c(-2e4, 6e4)) +
    scale_x_descrete(values=c(FALSE,TRUE), labels=c("Partial","Complete")) +
    guides(fill = guide_legend(title = "FUCK", label.position = "right")) 
    

#     p <- ggplot(cashLeft, aes(Week)) +
#         labs(title="Cash Burn Down", x="Week/Month", y="Remaining Cash") +
#         geom_area(fill="honeydew", colour="honeydew3", aes(y=Predicted)) +
#         geom_vline(
#             xintercept = as.numeric(mdy(paste(4:11,"/1/2015")) - mdy("1/1/2015"))/7, 
#             linetype=3, colour="wheat4") +
#         annotate("text", y = -1.5e4, size=rel(8), colour="steelblue4",
#                  x = as.numeric(mdy(paste(4:10,"/1/2015")) - mdy("1/1/2015"))/7 +
#                      as.numeric(mdy(paste(5:11,"/1/2015")) - mdy(paste(4:10,"/1/2015")))/14,
#                  label = month(4:10,label=TRUE)) +
#         annotate("segment", x=zeroCashWeek,  xend=zeroCashWeek, y=1.75e4, yend=0, colour="firebrick") +
#         annotate("text", y = 2e4, x = zeroCashWeek, size=6, colour="firebrick4",
#                  label = paste( month(zeroCashDate), day(zeroCashDate), year(zeroCashDate), sep="/") ) +
#         geom_bar(colour="black", stat="identity", alpha=0.25, aes(y=Actual), fill="green4") +
#         theme(
#             plot.title = element_text(size=rel(2.5), face="bold" ),
#             panel.grid.major.x = element_blank(), 
#             panel.grid.minor.x = element_blank(),
#             axis.title = element_text(size=rel(2), face="bold"), 
#             axis.text = element_text(size=rel(1.5), colour="steelblue4") ) +
#         scale_y_continuous(labels = dollar) +
#         coord_cartesian(xlim = c(10,45), ylim = c(-2e4, 6e4))
    
    print(p)
    
    # Return the cash burn down table
    cashLeft
}