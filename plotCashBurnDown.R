plotCashBurnDown <- function( trans ) {
    
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
    # Several date/time related calculations:
    #   * Adjust the week numbers so that week starts on Sunday
    #   * Find the earlies and latest weeks in the data
    #   * Compute current week number as both an integer and fractional value
    #   * Compute a fractional week value corresponding to the current date
    trans <- mutate(trans, Week = week(trans$Date - 4*24*60*60) %% 52)
    startWeek <- min(trans$Week)
    latestWeek <- max(trans$Week)
    currentWeek <- week(now() - 4*24*60*60) %% 52
    currentWeekFrac <- as.numeric(now() - mdy("1/1/2015") - 4) / 7
    todayFrcWk <- as.numeric(now() - mdy("1/1/2015")) / 7
    
    # ------------------------------------------------------------------------
    # Create a table containing the weekly income and expense totals for every
    # week 
    allWks <- data.frame( 
        Week=min(trans$Week) : max(trans$Week[trans$Type != "TRANSFER"]) )
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
    # Add to this table the actual cash remaining as a function of week
    cashLeft$Actual <- NA
    cashLeft$Actual[1] <- startCashInBank
    for (n in 1:nrow(tblIncExp)) {
        cashLeft$Actual[n+1] <- 
            cashLeft$Actual[n] + tblIncExp$Income[n] - tblIncExp$Expense[n]        
    }
    
    # ------------------------------------------------------------------------
    # Add to this table, the predicated cash remaining as a function of week
    cashLeft$Predicted[1] <- startCashInBank
    rowIdx <- nrow(tblIncExp) + 1
    for (n in 1:rowIdx) {
        cashLeft$Predicted[n+1] <- cashLeft$Actual[n] - estBurnRateWk
    }
    rowIdx <- rowIdx + 1
    cashLeft$Predicted[rowIdx] <- 
        cashLeft$Predicted[rowIdx] - 
        sum(estExpenses$Amount[is.na(estExpenses$DateIncurred)])
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
    # GENERATE THE PLOT OBJECT:
    #   The predicated and actual cash remaining vs time

    cashLeft <- mutate( cashLeft, partialWeek=(Week >= currentWeek) )
    x_axis_weeks <- c(15,25,35,45)

    # This plot will use the "cashLeft" table.  Set the x-axis to use the "Week"
    # column of that table now.
    p <- ggplot( cashLeft, aes(x=Week) ) +
    
        # Add the predicted cash burn down to the plot as a solid filled display
        # that will lie under the actual burn down display.
        geom_area( 
            fill="white", colour="honeydew3", alpha=0.5, 
            aes(x=Week, y=Predicted)) +
    
        # Add the actual cash burn down to the plot as a bar plot of weekly
        # values.  Weeks not yet frozen (i.e. in the past) are differenciated
        # from the others.
        geom_bar( 
            colour="black", stat="identity", alpha=0.25, 
            aes(y=Actual, fill=!partialWeek)) +

        # Add vertical grid lines on month boundaries and place the name of
        # each month between these boundaries.
        geom_vline(
            xintercept=as.numeric(mdy(paste(4:11,"/1/2015")) - mdy("1/1/2015"))/7, 
            linetype=3, colour="wheat4") +
        annotate( 
            "text", size=rel(8), colour="steelblue4", y=-1.7e4, 
            x = as.numeric(mdy(paste(4:10,"/1/2015")) - mdy("1/1/2015"))/7 +
                as.numeric(mdy(paste(5:11,"/1/2015")) - mdy(paste(4:10,"/1/2015")))/14,
            label = month(4:10,label=TRUE)) +
    
        # Add a line and label to the plot corresponding to the current date
        # and corresponding acutal cash amount.
        annotate(
            "segment", x=todayFrcWk, xend=todayFrcWk, 
             y=-5e3, yend=6e4, colour="firebrick", linetype=3, size=rel(1)) +
        annotate(
            "text", y=-8e3, x=todayFrcWk, size=6, face="bold", 
            colour="firebrick4", label="today", sep="/") +
        annotate(
            "text", y=-1.2e4, x=todayFrcWk, size=6, face="bold",
            colour="firebrick4", 
            label=dollar( round(filter(cashLeft, Week==20)$Actual)) ) +
    
        # Add a line and label to the plot corresponding the predicted zero
        # cash remaining day.
        annotate(
            "segment", x=zeroCashWeek,  xend=zeroCashWeek, 
             y=-5e3, yend=6e4, colour="firebrick", linetype=3, size=rel(1)) +
        annotate(
            "text", y=-8e3, x=zeroCashWeek, size=6, face="bold", 
            colour="firebrick4", 
            label=paste( month(zeroCashDate,label=TRUE), day(zeroCashDate)) ) +
        annotate(
            "text", y=-1.2e4, x=zeroCashWeek, size=6, face="bold",
            colour="firebrick4", label="$0" ) +
        
        # Add titles for the plot, x-axis, and y-axis.
        labs(
            title=paste( "Cash Burn Down - ", month(today(),label=TRUE), " ",
                         day(today()), ", ", year(today()), sep="" ), 
            x="Week/Month", 
            y="Remaining Cash") +
    
        theme(
            # Adjust the plot title settings
            plot.title = element_text(size=rel(2.5), face="bold", vjust=rel(1.0) ),
        
            # Turn off the x gridlines (in weeks)
            panel.grid.major.x = element_blank(), 
            panel.grid.minor.x = element_blank(),
        
            # Adjust the y gridline settings
            panel.grid.major.y = element_line(linetype=3, colour="wheat4"),
            panel.grid.minor.y = element_line(linetype=3, colour="wheat4"),
        
            # Adjust the x and y axis settings
            axis.title.y = element_text(size=rel(2), face="bold", vjust=rel(1.0)), 
            axis.title.x = element_text(size=rel(2), face="bold", vjust=rel(-0.5)), 
            axis.text = element_text(size=rel(1.5), colour="steelblue4"),
            axis.ticks = element_line(size=rel(2)),
        
            # Turn off display of the legend
            # todo:  This is not right, but better than nothing
            legend.title = element_blank(),
            legend.text = element_blank(),
            legend.key.size = unit(0,"cm") ) +
    
        # Adjust the y-axis display so it's values are shown as dollar amounts.
        # (this takes advantage of the dollar function from the scales package)
        scale_y_continuous( labels=dollar ) +
    
        # Set the x-axis tick values to display as "wk-XX" where XX is the
        # corresponding week number.
        scale_x_continuous( 
            breaks=x_axis_weeks, labels=paste("wk-",x_axis_weeks,sep="") ) +
    
        # Adjust the x and y axis limits.
        coord_cartesian(
            xlim=c( 10, 45 ), 
            ylim=c( -2e4, 6e4 ) )

    # Display the plot.
    print(p)
    
    # Return the cash burn down table
    cashLeft
}