## Setup
source("server.R")
## Specify layout
shinyUI(pageWithSidebar(
  headerPanel(HTML("Mortgage Comparison Tool")),
  sidebarPanel(
    ## Construct input options
    ## Enter mortgage information
    h4("Mortgage A parameters"),
    ## Loan amount
    numericInput("PrincipalA", "Amount to be borrowed (in $)", 100000, min=1),
    ## Interest rate
    numericInput("interestA", "Yearly interest rate (in %)", 5, min=0.01, max=100, step=0.01),
    ## Loan duration
    numericInput("durationA", "Loan duration (in years)", 20, min=1/12, max=100, step=1/12),
    ## Enter mortgage information
    h4("Mortgage B parameters"),
    ## Loan amount
    numericInput("PrincipalB", "Amount to be borrowed (in $)", 100000, min=1),
    ## Interest rate
    numericInput("interestB", "Yearly interest rate (in %)", 5, min=0.01, max=100, step=0.01),
    ## Loan duration
    numericInput("durationB", "Loan duration (in years)", 20, min=1/12, max=100, step=1/12)
  ),
  mainPanel(
    h4("Mortgage A"),
    verbatimTextOutput("PaymentA"),
    h4("Mortgage B"),
    verbatimTextOutput("PaymentB"),
    h4("Mortgage A and B Visualization"),
    p("The following interactive graph allows you to choose which data to view and from which of the two mortgage options (the data options for each mortgage is appended by '_A' or '_B' respectively). Clicking on a solid circle beside a data option turns that option off. Clicking on an open circle turns that option on. Also clicking on 'grouped' or 'stacked' will choose that graphing option."),
    showOutput("myChartAB", "nvd3")
    )
  )
)
