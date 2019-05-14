library(shiny)
library(ggplot2)

edu = read.csv("../../states_all.csv",stringsAsFactors=FALSE)
edu = edu[edu$STATE %in% unique(edu$STATE)[1:51],-1]
edu = edu[edu$YEAR != 2017,]
edu$STATE = tolower(gsub("_"," ",edu$STATE))

ui <- fluidPage(

    titlePanel("US Education"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "year",
                        label = "Select a year",
                        min = 1992, max = 2016,
                        value = 2016, sep=""),
            selectInput('x', 'Select the variable', 
                        names(edu)[-(1:2)], "TOTAL_REVENUE")
        ),

        mainPanel(
           plotOutput("histPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderPlot({
        dat = subset(edu, YEAR==input$year)
        dat = dat[,c("STATE",input$x)]
        missProp = mean(is.na(dat[,2]))
        if (missProp>0.95) {
            showModal(modalDialog(
                title = "Important message",
                "Too many missing values. Please select a different variable!",
                easyClose = TRUE
            ))
            return()
        }
        names(dat) = c("state","x")
        ggplot(data=dat,aes(x)) + geom_histogram(bins=15) +
            xlab(input$x) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
