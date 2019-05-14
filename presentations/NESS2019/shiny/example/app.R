#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
            selectInput('x', 'Select the x variable', 
                        names(edu)[-(1:2)], "FEDERAL_REVENUE"),
            selectInput('y', 'Select the y variable', 
                        names(edu)[-(1:2)], "LOCAL_REVENUE")
        ),

        mainPanel(
           plotOutput("ScatterPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ScatterPlot <- renderPlot({
        dat = subset(edu, YEAR==input$year)
        dat = dat[,c("STATE",unique(c(input$x,input$y))),drop=FALSE]
        missProp = sapply(dat,function(x){mean(is.na(x))})
        if (ncol(dat)<3) {
            showModal(modalDialog(
                title = "Important message",
                "Please choose a different column!",
                easyClose = TRUE
            ))
            return()
        } else if (any(missProp>0.95)) {
            showModal(modalDialog(
                title = "Important message",
                paste("Too many missing values in column",
                        which(missProp>0.95)-1),
                easyClose = TRUE
            ))
            return()
        }
        names(dat) = c("state","x","y")
        types = sapply(dat,function(x){is.numeric(x)})[-1]
        if (all(types)){
            ggplot(data=dat,aes(x=x,y=y,label=state)) +
                geom_point() + geom_text(check_overlap=TRUE) +
                xlab(input$x) + ylab(input$y) 
        } else if (all(!types)){
            newdat = table(dat)
            s = data.frame(expand.grid(attr(newdat,"dimnames")$x,
                                       attr(newdat,"dimnames")$y))
            s$value = as.vector(newdat)
            ggplot(data=s,aes(x=Var1,y=value,fill=Var2))+
                geom_bar(stat='identity') + xlab(input$x) +
                ylab("Count") + labs(fill=input$y)
        } else {
            ggplot(data=dat,aes(x=x,y=y))+geom_boxplot() +
                xlab(input$x) + ylab(input$y) 
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
