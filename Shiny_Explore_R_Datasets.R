library(shiny)
library(DT)
d <- data(package="datasets")
d.list <- d$results[,"Item"] # read dataset names
indices.nobrackets <- !grepl("[(]",d.list)
d.list <- d.list[indices.nobrackets] # removing redundant datasets containing '(' in the names
d.title <- d$results[indices.nobrackets,"Title"] #read title information

ui <- shinyUI(fluidPage(
        titlePanel("Explore R Datasets"),
        sidebarLayout(
                sidebarPanel(width = 4,
                        h4("Usage"),
                        h5("This app let's you explore built-in R datasets
                           quickly."),
                        tags$ul(
                          tags$li("Choose a dataset from dropdown 'Choose a                                           Dataset'"),
                          tags$li("Brief description of dataset is displayed
                                  in the side panel."),
                          tags$li("Selected data set will be displayed in 
                                  the main panel using data table."),
                          tags$li("The table is interactive, so you can explore
                                  the data by filtering, sorting and displaying
                                  more or less number of observations."),
                          tags$li("A histogram will be drawn on the side
                                  panel."),
                          tags$li("If the dataset contains multiple varaibles,
                                  you can select the variable to plot using
                                  the second dropdown,'Choose Variable to
                                  Plot'.")
                        ),
                        selectInput("dataset",
                                    h4("Choose a Dataset:"),
                                    choices=d.list),
                        h4("Description of Dataset"),
                        verbatimTextOutput("title"),
                        uiOutput("vars"),
                        plotOutput("histo")
                ),
                mainPanel(
                        dataTableOutput("view")
                )
        )
))

server <- shinyServer(
        function(input,output){

                datasetInput <- reactive({
                        as.data.frame(get(input$dataset))
                })

                output$view <- renderDataTable({
                        datasetInput()
                })
                output$vars <- renderUI({
                        dataset <- datasetInput()
                        var.names <- names(dataset)
                        selectInput("varnames",
                                    h4("Choose Variable to Plot"),
                                    choices = var.names)
                })
                output$histo <- renderPlot({
                        dataset <- datasetInput()
                        with(dataset, hist(get(input$varnames),
                                          col = "dodgerblue4",
                                          xlab = "",
                                          main = input$varnames))
                })
                output$title <- renderPrint({
                        cat(d.title[which(d.list %in% input$dataset)])
                })
        }
)

shinyApp(ui,server)