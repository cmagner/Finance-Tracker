plotCashBurnDown <- function( trans ) {

    # Adjust the week numbers so that week starts on Sunday
    trans <- mutate(trans, Week = week(trans$Date - 4*24*60*60) %% 52)
#    startWeek <- week(min(trans$Date))
    startWeek <- min(trans$Week)
    currentDate <- max(trans$Date)        
    
    # Create a table containing the weekly income and expense totals for every
    # week to date
    #allWks <- data.frame( Week=min(week(trans$Date)) : max(week(trans$Date)) )
    allWks <- data.frame( Week=min(trans$Week) : max(trans$Week) )
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
    
    #     Starting Bank Balances (as of 3/15):    
    #       Clearview               $5,600 
    #       Fulton - personal	    $2,867 
    #       Fulton - business   	$563 
    #                       Total	$9,030 
    #     
    #     TruePostition Lay-Off Money:	
    #       Net Vacation Pay        $2,036 
    #       Net Severance Pkg	    $63,442 
    #                       Total	$65,478 
    startCashInBank <- 5600 + 2867 + 563
    cashFromTP <- 2036 + 63442
    
    #     Estimated Extraordinary Expenditures (known):    
    #       Hawaii Vacation	        $10,000 
    #       Retaining Wall	        $2,500 
    #       New Couch	            $1,000 
    #                       Total	$13,500 
    estExpenses <- tbl_df( data.frame(
        Expense = c("Hawaii Vacation", "Retaining Wall", "New Couch"),
        Amount = c(10000, 2500, 1000),
        DateIncurred = c(mdy("3/24/2015"), NA, NA)) )
    
    #     Income Rates (per week):	
    #       Trueposition Net Salary $1,628 
    #       Magner Com Gross Inc.   $473 
    #                       Total	$2,101 
    #     
    #     Paid for from TruePosition Gross (per week):	
    #       Health Insurance	    $360 
    #       HSA Savings	            $113 
    #                       Total	$473 
    estBurnRateWk <- sum(c(1628, 473, 360, 113))
    
    # Initialize a table for actual, estimated, and predicted cash remaining as
    # a function of week
    cashLeft <- tbl_df(data.frame(Week = startWeek:week(mdy("12/31/2015"))))
    
    # Create the actual cash remaining as a function of week
    cashLeft$ActCash <- NA
    cashLeft$ActCash[1] <- startCashInBank
    for (n in 1:nrow(tblIncExp)) {
        cashLeft$ActCash[n+1] <- cashLeft$ActCash[n] + tblIncExp$Income[n] - tblIncExp$Expense[n]        
    }
    
    # Generate the estimated cash remaining
    cashLeft$EstCash[1] <- startCashInBank + cashFromTP - sum(estExpenses$Amount) 
    for (n in 2:nrow(cashLeft)) { 
        cashLeft$EstCash[n] <- cashLeft$EstCash[n-1] - estBurnRateWk 
    }
    for (n in 1:nrow(estExpenses)) {
        selector <- 
            if (!is.na(estExpenses$DateIncurred[n])) 
                cashLeft$Week <= (week(estExpenses$DateIncurred[n] - 4*24*60*60) %% 52)
        else 
            cashLeft$Week < (week(currentDate - 4*24*60*60) %% 52)
        cashLeft$EstCash[selector] <- 
            cashLeft$EstCash[selector] + estExpenses$Amount[n]
    }
    
    # Create the predicated cash remaining as a function of week
    cashLeft$Predicted[1] <- startCashInBank
    rowIdx <- nrow(tblIncExp) + 1
    for (n in 1:rowIdx) {
        cashLeft$Predicted[n+1] <- cashLeft$ActCash[n] - estBurnRateWk
    }
    rowIdx <- rowIdx + 1
    cashLeft$Predicted[rowIdx] <- 
        cashLeft$Predicted[rowIdx] - sum(estExpenses$Amount[is.na(estExpenses$DateIncurred)])
    for (n in rowIdx:(nrow(cashLeft)-1)) {
        cashLeft$Predicted[n+1] <- cashLeft$Predicted[n] - estBurnRateWk
    }
    
    # Plot the estimated, actual and predicated cash remaining
    fit<-coef(lm(cashLeft$Predicted[rowIdx:nrow(cashLeft)] 
               ~ cashLeft$Week[rowIdx:nrow(cashLeft)]))
    zeroCashWeek <- -fit[1]/fit[2]
    zeroCashDate <- mdy("1/1/2015")
    second(zeroCashDate) <- ((zeroCashWeek*7) + 4)*24*60*60

#        geom_vline( xintercept = zeroCashWeek, ymax=3e4, colour="red") +
p <- ggplot(cashLeft, aes(Week)) +
        labs(title="Cash Burn Down", x="Week/Month", y="Remaining Cash") +
        geom_area(fill="honeydew", colour="honeydew3", aes(y=Predicted)) +
        geom_vline(
            xintercept = as.numeric(mdy(paste(4:11,"/1/2015")) - mdy("1/1/2015"))/7, 
            linetype=3, colour="wheat4") +
        annotate("text", y = 7e4, size=6,
                 x = as.numeric(mdy(paste(4:10,"/1/2015")) - mdy("1/1/2015"))/7 +
                     as.numeric(mdy(paste(5:11,"/1/2015")) - mdy(paste(4:10,"/1/2015")))/14,
                 label = month(4:10,label=TRUE)) +
        annotate("segment", x=zeroCashWeek,  xend=zeroCashWeek, y=-1e4, yend=2e4, colour="firebrick") +
        annotate("text", y = 2.5e4, x = zeroCashWeek, size=6, colour="firebrick4",
                 label = paste(month(zeroCashDate),day(zeroCashDate),year(zeroCashDate),sep="/")) +
        geom_bar(colour="black", stat="identity", fill="green4", alpha=0.3, aes(y=ActCash)) +
        coord_cartesian(xlim = c(10,45), ylim = c(-1e4, 7.5e4))
    print(p)
#     p <- ggplot(cashLeft, aes(Week)) +
#         geom_vline(
#             xintercept = as.numeric(mdy(paste(4:11,"/1/2015")) - mdy("1/1/2015"))/7, 
#             linetype=3, colour="wheat4") +
#         annotate("text", y = 7e4, 
#                  x = as.numeric(mdy(paste(4:10,"/1/2015")) - mdy("1/1/2015"))/7 +
#                      as.numeric(mdy(paste(5:11,"/1/2015")) - mdy(paste(4:10,"/1/2015")))/14,
#                  label = month(4:10,label=TRUE)) +
#         geom_area(fill="honeydew", colour="honeydew3", alpha=0.5, aes(y=EstCash)) +
#         geom_bar(colour="black", stat="identity", fill="green4", alpha=0.7, aes(y=ActCash)) +
#         geom_line(colour="red", linetype=3, size=0.5, aes(y=Predicted)) +
#         geom_point(colour="red4",size=7.5, shape="$", aes(y=Predicted)) +
#         coord_cartesian(xlim = c(10,45), ylim = c(-1e4, 7.5e4))
#     print(p)
    
    # Return the cash burn down table
    cashLeft
}