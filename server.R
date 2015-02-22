## Setup
library("shiny")
library("rCharts")
library("reshape2")

options("scipen" = 20) # this is so that numbers will not display in scientific notation

# Monthly payment calculation
pay <- function(Principal, interest, duration, payfreq=1, firstpay=1, compoundfreq=1) {
  r <- interest / (100 * 12 / compoundfreq )
  if(firstpay > 1) {
    Principal <- Principal * (1 + r)^((firstpay - 1) / compoundfreq)
    duration <- duration - (firstpay - 1) / 12
  }
  Payment <- Principal * r / ( 1 - ( 1 + r)^(-duration * 12 / compoundfreq) ) * payfreq / compoundfreq
  res <- list(r=r,Payment=Payment, Principal=Principal)
  return(res)
}
## Amortization table...have old parameters in here for payment frequency, etc...will clean out eventually
amort <- function(Principal, interest, duration, payfreq=1, firstpay=1, compoundfreq=1) {
  pay <- pay(Principal, interest, duration)
  data <- data.frame(month = seq(0, duration * 12))
  data$Payment <- 0
  data$Payment[ (data$month - firstpay) >= 0 & (data$month - firstpay) %% payfreq == 0 ] <- pay$Payment
  i <- which(data$Payment != 0)
  i <- i[length(i)]
  data$Payment[ i ] <- 0
  data$Payment[ i ] <- pay$Payment * (duration - (firstpay - 1) / 12) * 12 / payfreq - sum(data$Payment)
  data$Total_Paid <- cumsum(data$Payment)
  data$Principal <- NA
  data$Principal[1] <- Principal
  idx <- (data$month - firstpay) >=0 & (data$month - firstpay) %% compoundfreq == 0
  idx.pr <- which(idx)[-length(idx)] + compoundfreq - 1
  if(any(idx.pr > max(data$month))) {
    idx.pr <- idx.pr[-which(idx.pr > max(data$month))]
  }
  if(firstpay > 1) {
    data$Principal[firstpay] <- pay$Principal
  }
  data$Principal[ idx.pr ] <- (1 + pay$r)^seq_len(length(idx.pr)) * pay$Principal - ( (1 + pay$r)^seq_len(length(idx.pr)) - 1 ) / pay$r * pay$Payment * compoundfreq / payfreq
  data$Principal[ nrow(data)] <- 0
  return(data)
}
## Main shiny function
shinyServer(function(input, output, session) {

	## Display Payment
	output$PaymentA  <- renderPrint({
		PaymentA <- pay(input$PrincipalA, input$interestA, input$durationA)$Payment
		cat("Your monthly payment is: $")
		cat(format(round(PaymentA, 2), decimal.mark=".", big.mark=",", nsmall = 2))
		cat(".\nTotal paid at end of loan is $")
		totalA <- PaymentA * input$durationA * 12
		cat(format(round(totalA, 2), decimal.mark=".", big.mark=",", nsmall = 2))
		cat(" of which $")
    cat(format(round(input$PrincipalA, 2), decimal.mark=".", big.mark=",", nsmall = 2))
    cat(" is principal and $")
		cat(format(round(totalA - input$PrincipalA, 2), decimal.mark=".", big.mark=",", nsmall = 2))
		cat(" is interest.")
	})
	output$PaymentB  <- renderPrint({
	  PaymentB <- pay(input$PrincipalB, input$interestB, input$durationB)$Payment
	  cat("Your monthly payment is: $")
	  cat(format(round(PaymentB, 2), decimal.mark=".", big.mark=",", nsmall = 2))
	  cat(".\nTotal paid at end of loan is $")
	  totalB <- PaymentB * input$durationB * 12
	  cat(format(round(totalB, 2), decimal.mark=".", big.mark=",", nsmall = 2))
	  cat(" of which $")
	  cat(format(round(input$PrincipalB, 2), decimal.mark=".", big.mark=",", nsmall = 2))
	  cat(" is principal and $")
	  cat(format(round(totalB - input$PrincipalB, 2), decimal.mark=".", big.mark=",", nsmall = 2))
	  cat(" is interest.")
	})
	## Make a chart with the data
	output$myChartAB <- renderChart({
	  dataA <- amort(input$PrincipalA, input$interestA, input$durationA)
	  dataB <- amort(input$PrincipalB, input$interestB, input$durationB)

    names(dataA) <- c("Month", "Payment_A", "Total_Paid_A", "Principal_A")
    names(dataB) <- c("Month", "Payment_B", "Total_Paid_B", "Principal_B")

	  dataA$Current_Interest_A[1] <- 0.0
	  dataA$Current_Principal_A[1] <- 0.0
	  for (idx in 2:nrow(dataA)) {
	    dataA$Current_Interest_A[idx] <- dataA$Payment_A[idx] - dataA$Principal_A[idx-1] + dataA$Principal_A[idx]
	    dataA$Current_Principal_A[idx] <- dataA$Payment_A[idx] - dataA$Current_Interest_A[idx]
	  }

    dataB$Current_Interest_B[1] <- 0.0
    dataB$Current_Principal_B[1] <- 0.0
    for (idx in 2:nrow(dataB)) {
      dataB$Current_Interest_B[idx] <- dataB$Payment_B[idx] - dataB$Principal_B[idx-1] + dataB$Principal_B[idx]
      dataB$Current_Principal_B[idx] <- dataB$Payment_B[idx] - dataB$Current_Interest_B[idx]
    }
  if(nrow(dataA) < nrow(dataB)) {
    lenA <- nrow(dataA)
    lenB <- nrow(dataB)
    for (idx in (lenA+1):lenB) {
      newrow <- c(idx, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
      dataA<- rbind(dataA, newrow)
    }
  }
if(nrow(dataB) < nrow(dataB)) {
  lenA <- nrow(dataA)
  lenB <- nrow(dataB)
  for (idx in (lenB+1):lenA) {
    newrow <- c(idx, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    dataB<- rbind(dataB, newrow)
  }
}
    dataAB <- merge(dataA, dataB)
	  dat.chartAB <- melt(dataAB, id = 'Month')
	  dat.chartAB$value <- round(dat.chartAB$value, 0)
	  chartAB <- nPlot(value ~ Month, group = 'variable', data = dat.chartAB[complete.cases(dat.chartAB), ], type = 'multiBarChart')
	  chartAB$xAxis(axisLabel = 'Month')
    chartAB$yAxis(axisLabel = 'Amount ($)')
	  chartAB$addParams(dom = 'myChartAB')
	  return(chartAB)
	})
})
