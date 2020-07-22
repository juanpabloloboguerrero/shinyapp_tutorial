#Shiny apps Coding Club tutorial
#Shiny = R package to create web-applications (commonly designed around data presentation and analysis) using R language


# Packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(agridat)


# Loading data ----
# Contains data needed to build the app
Barley <- as.data.frame(beaven.barley)


# ui.R ----
# ui object contains information about the layout of the app as it will appear in the browser

ui <- 
    fluidPage( # defines a layout that will resize depending on the size of the broswer
        titlePanel("Barley Genotypes Yield"), # title of app
        sidebarLayout( # app will have sidebar layout
            position = "right",
            sidebarPanel(h3("Inputs for histogram"), # app will have sidebar panel (ex. to put sliders, text input boxes, etc.)
                         selectInput("gen", # name of input for reference in code (won't be displayed in app)
                                     label = "1. Select genotype", # label of sidebar panel that will be displayed in the app
                                     choices = c("A" = "a","B" = "b","C" = "c","D" = "d","E" = "e","F" = "f","G" = "g","H" = "h"),# choices that can be selected from the sidebar panel 
                                     selected = "a"),# value from dropdown menu that will be selected by default
                         br(),
                         selectInput("col", 
                                     label = "2. Select histogram colour", 
                                     choices = c("blue","green","red","purple","grey"), 
                                     selected = "blue"),
                         br(),
                         sliderInput("bin", "3. Select number of histogram bins", min=1, max=25, value= c(10)),
                         br(),
                         textInput("text", "4. Yield variations among Barley genotypes", "")),
            mainPanel( # adding output objects specified in server section so that they appear on the app's main panel
                plotOutput("myhist"),
                tableOutput("mytable"),
                textOutput("output$mytext")
            )
        )
    )



# Server.R ---- (as specified in ui object)
#  contains information about the computation of the app, creating plots, tables, maps, etc. using information provided by the user of the app 

server <- function(input, output) {
    output$myhist <- renderPlot(ggplot(Barley, aes(x = yield)) + # creating renderPlot object with ggplot inside it
                                    geom_histogram( # adding a histogram to the plot
                                        bins = input$bin, #makes number of bins in histogram be the same as specified by input$bins 
                                        fill = input$col, # makes colour of histogram bars be the same as specified by input$colour 
                                        group=input$gen, #makes histogram only display data where the gen value is the same as the value specified by input$gen
                                                   data=Barley[Barley$gen == input$gen,],
                                                   colour = "black")) # outline of histogram bins set to colour black
    
    output$mytext <- renderText(input$text) # making text match the input text specified
    
    output$mytable <- renderTable(Barley %>% # creating table that displays summary statistics of specific barley genotype chosen
                                      filter(gen == input$gen) %>%
                                      summarise("Mean" = mean(yield), 
                                                "Median" = median(yield),
                                                "STDEV" = sd(yield), 
                                                "Min" = min(yield),
                                                "Max" = max(yield)))
}


# Run the app ----
# command used to run the app (indicates that the user interface comes from "ui" object and that the server information comes from the "server" object)

shinyApp(ui = ui, server = server) 

