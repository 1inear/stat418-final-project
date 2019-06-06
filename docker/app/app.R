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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("movie gross income prediction by multiple linear regression"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("log_Votes",
                        "Select log_Votes:",
                        min = 4.9,
                        max = 14.5,
                        value = 10),
            sliderInput("Score",
                        "Select Movie Score:",
                        min = 1,
                        max = 9.8,
                        value =6),
            radioButtons("new_Year", "Select Year:",
                         list("before2000" = "before2000",
                              "2000-2010" = "first_ten",
                              "after 2010" = "second_ten")),
            radioButtons("Level", "Select movie level:",
                         list("PG" = "PG",
                              "PG-13" = "PG-13",
                              "R" = "R")),
            radioButtons("final_Duration", "Select movie duration:",
                         list(">120 min" = "Long",
                              "101-120 min" = "Normal",
                              "<=100 min" = "Short")),
            radioButtons("Animation", "Select movie genre: Animation",
                         list("include" = 1,
                              "not include" = 0)),
            radioButtons("Comedy", "Select movie genre: Comedy",
                         list("include" = 1,
                              "not include" = 0)),
            radioButtons("Drama", "Select movie genre: Drama",
                         list("include" = 1,
                              "not include" = 0)),
            radioButtons("Sport", "Select movie genre: Sport",
                         list("include" = 1,
                              "not include" = 0))
            
            ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderTable({
        data<-read.csv("newmovie.csv")
        data$Animation = as.factor(data$Animation)
        data$Comedy = as.factor(data$Comedy)
        data$Drama = as.factor(data$Drama)
        data$Sport = as.factor(data$Sport)
        mfinal2<-lm(formula = log_gross ~ new_Year + Level + final_Duration 
        + Score + log_Votes + Animation + Comedy + Drama + Sport, data = data)
        parameterset<-data.frame(new_Year=input$new_Year, Level=input$Level, final_Duration=input$final_Duration,
        Score=input$Score, log_Votes=input$log_Votes, Animation=input$Animation,
        Comedy=input$Comedy, Drama=input$Drama, Sport=input$Sport )
        result<-exp(predict(mfinal2, parameterset))
        result
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

