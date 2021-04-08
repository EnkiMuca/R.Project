library(shiny)

#Focussed on tools for working with data frames
library(dplyr, warn.conflicts = FALSE)

#Provides an R interface to the JavaScript library DataTables
library(DT, warn.conflicts = FALSE)

#A system for creating graphics, main focus on visualisation
library(ggplot2)

#Reshape data to present them better (matrix)
library(reshape2)

#Provide methods for automatically determining breaks and labels for axes and legends
library(scales)

#Prevent scientific notation to make numbers easily for people
options(scipen = 999)





###################################################################################
#Now we will start designing the layout of the app, "Front End".

#hr()  --->   Horizontal Rule [line]
#br()  --->   Line Break
###################################################################################


ui <- fluidPage(
    
    
    
tags$head(
tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');

body {
color: black;
}

h2 {
font-family: 'Yusei Magic', sans-serif;
padding: 40px;
text-align: center;
background: #B3B6B7;
color: white;
font-size: 30px;
}
      
h3 {
font-family: 'Yusei Magic', sans-serif;
padding: 10px;
text-align: center;
background: #B3B6B7;
color: white;
font-size: 15px;
}
      
h4 {
font-family: 'Courier New', monospace;
padding: 10px;
text-align: center;
background: #B3B6B7;
color: white;
font-size: 20px;
}
      
.shiny-input-container
{
color: #474747;
}

"))),
    
    
    
    
#Put the app's name at the beginning
titlePanel("Student Loans with EnkiApp"),
    
HTML('<p style=" color: navy;
text-indent: 30px;">This app will allow you to take a student loan in just a click. All you need to do is to put the details of the loan and the system will
calculate each payment you will need to do in the future. There will be a graphic layout where you will see the composure of your monthly pamynet between the principal
and the interest and below that, a table will give you the full written details of each payment.</p>'),
    


#Creating a sidebar block, similar to a container in HTML
sidebarLayout(
sidebarPanel(
            
#What are the details of the loan you are asking for, the default value in the system, what is the minimum you can ask and what is the step of increase/decrease button?
numericInput("principal", "How much do you need? ($)", 5000, min = 0, step = 200),
hr(),
numericInput("interest", "Put the annual interest rate (%)", 2, min = 0, max = 100, step = 0.01),
hr(),
sliderInput("length", "How many years do you need to repay it?",
min = 0,
max = 10,
value = 5,
step = 1),
hr(),
            
#Give a graphic layout to the data table, change to "FALSE" if not desired
checkboxInput("plot", "Display plot?", TRUE),),
        


#Main block where the output of the above choices will be displayed
mainPanel(
            
#First thing that will be displayed in the mainPanel. It will get be generated through the renderUI function in the server part.
uiOutput("first"),
br(),
            
#Second element that will be displayed in the mainPanel, but this time is a plot. It will get be generated through the renderPlot function in the server part.
plotOutput("second"),
br(),
            
#Third element that will be displayed in the mainPanel, but this time is a table It will get be generated through the renderDataTable function in the server part.
DT::dataTableOutput("third"),
br(),
            
#A relatively small header to put my name and the link to the university email
h3(tags$a(href = "enki.muca@studenti.unimi.it", "Enki Muca")),
)))


###################################################################################
#Now we will start function code of the app, "Back End".

#hr()  --->   Horizontal Rule [line]
#br()  --->   Line Break

#Monthly Payments= P[r(1+r)^n/((1+r)^n)-1)]
###################################################################################


#This Code Snippet in the six following rows is taken from a standart loan formula app, it turns the interest rate and other details into correct form to calculate the monthly payment.
#It practically gives a logic to the server how should it generally work for all slider examples.

server <- function(input, output) {
loan <- function(P = 500000, I = 6, L = 30, amortization = TRUE, plotData = TRUE) {
J <- I / (12 * 100)
N <- 12 * L
M <- P * J / (1 - (1 + J)^(-N))
monthPay <<- M
        
       
#############################################
# J --> Monthly Interest Percentage         #
#                                           #
# N --> Number of months                    #
#                                           #
# M --> General formula of monthly payment  #
#                                           #
#############################################


#Calculate the amortization of the loan every month, if we decide to give a loan without amortization need to change the above line to "amortization = FALSE"
if (amortization == TRUE) {
Pt <- P # current principal or amount of the loan
currP <- NULL
while (Pt >= 0) {
H <- Pt * J # this is the current monthly interest
C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
Q <- Pt - C # this is the new balance of your principal of your loan
Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
currP <- c(currP, Pt)}

monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
aDFmonth <<- data.frame(
Month = 1:length(currP),
Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
Balance = c(currP[1:(length(currP))]),
Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
Principal = monthP,
Interest = c((monthPay - monthP)[1:(length(monthP))]))

aDFmonth <<- subset(aDFmonth, Year <= L * 12)
aDFyear <- data.frame(
  
Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
Year = as.factor(na.omit(unique(aDFmonth$Year))))

aDFyear <<- aDFyear
}
if (plotData == TRUE) {
aDFyear2 <- aDFyear %>%
rename(
Interest = Annual_Interest,
Payment = Annual_Payment,
Principal = Annual_Principal
)

aDFyear2$Year <- as.factor(aDFyear2$Year)
aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
            
ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
geom_bar(position = "fill", stat = "identity") +
labs(y = "Payment") +
scale_y_continuous(labels = percent) +
theme_minimal() +
theme(legend.title = element_blank(), legend.position = "top")
}}
    

#It gives a summarize of all loan details in form of a list. It gets the data from the input given in the sidebar panel.
output$first <- renderUI({
loan(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
HTML(paste0(
HTML('<hr style="color: purple; border: 5px solid black;">'),
"<b><i><h4>", "Loan Details", "</h4>",
"<ul><li>Your loan <b>==></b> ", format(round(input$principal, 2), big.mark = " ,"), "$</li>",
"<br>",
"<li>Annual interest rate <b>==></b> ", input$interest, "%</li>",
"<br>",
"<li>Duration: ", input$length, " years <b>==></b> (", input$length * 12, " months)</li>",
"<br>",
"<li><b>", "Monthly payment <b>==></b> ", format(round(monthPay, digits = 2), big.mark = ","), "$</b></li>",
"<br>",
"<li><b>", "Loan + Interest <b>==></b> ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "$</b></i></b> </li></ul>",
HTML('<hr style="color: purple; border: 5px solid black;">')
))})


#It is called by "second" in UI. It gives all the information needed to display the plot.
output$second <- renderPlot({
loan(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
})


#It is called by the "third" from UI. It gives a table that will display all the given data of the loan for each payment. 
output$third <- DT::renderDataTable({
loan(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
df_month <- DT::datatable(data.frame(round(aDFmonth, 2)),
     
                                                       
#It decorates the table with buttons that offer the option of downloading the data to some specific platforms given below in the collection
extensions = "Buttons",
options = list(
lengthChange = TRUE,
dom = "Blrtip",
buttons = c("copy", "csv", "excel", "pdf", "print"),
  

#It gives the option to display the table by a number of rows, first one will be the default option.                                    
lengthMenu = list(c(10, -1, 12, 15, 25, 50, 100), c("10", "All", "12", "15", "25", "50", "100"))
),
rownames = FALSE
) %>%
  
  
#It gives the currency to the table cells and where is to be put the "," in a number.
formatCurrency(c("Balance", "Payment", "Principal", "Interest"), currency = "$", interval = 3, mark = ",")
})
}


###################################################################################
# Run the application
shinyApp(ui = ui, server = server)
###################################################################################
