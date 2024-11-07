#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

source("R/compute_desplacement.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # # Application title
    # titlePanel("Dynamic Response of a spring immersed in a fluid", windowTitle="Dynamic response simulation"),
    # 
    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("mass",
    #                     "Mass, kg",
    #                     min = 0.1,
    #                     max = 10,
    #                     value = 1),
    #         sliderInput("damping",
    #                     "Damping coefficient, kg/s",
    #                     min = 0.1,
    #                     max = 100,
    #                     value = 5),
    #         sliderInput("stiff",
    #                     "Stiffness coefficient, kg/s2",
    #                     min = 0.1,
    #                     max = 1,
    #                     value = 0.1),
    #         sliderInput("force",
    #                     "Max input force, N",
    #                     min = 0.1,
    #                     max = 10,
    #                     value = 1),
    #         sliderInput("frequency",
    #                     "Frequency, radian/s",
    #                     min = 0.1,
    #                     max = 25,
    #                     value = 1),
    #         sliderInput("maxtime",
    #                     "Response duration, minutes",
    #                     min = 1,
    #                     max = 300,
    #                     value = 10)
    #     ),

        # Show the dynamic system response
        # mainPanel(
        #    plotOutput("response")),
        # )

    theme =  shinythemes::shinytheme("flatly"),
    #  "shinythemes",
    titlePanel("Dynamic Response of a spring immersed in a fluid", windowTitle="Dynamic response simulation"),
    h4("Prepared by Pablo Adames on November 7, 2024"),
    sidebarLayout(
      sidebarPanel(
          sliderInput("mass",
                      "Mass, kg",
                      min = 0.1,
                      max = 10,
                      value = 1),
          sliderInput("damping",
                      "Damping coefficient, kg/s",
                      min = 0.1,
                      max = 100,
                      value = 5),
          sliderInput("stiff",
                      "Stiffness coefficient, kg/s2",
                      min = 0.1,
                      max = 1,
                      value = 0.1),
          sliderInput("force",
                      "Max input force, N",
                      min = 0.1,
                      max = 10,
                      value = 1),
          sliderInput("frequency",
                      "Frequency, radian/s",
                      min = 0.1,
                      max = 25,
                      value = 1),
          sliderInput("maxtime",
                      "Response duration, minutes",
                      min = 1,
                      max = 300,
                      value = 10)
      ),
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Response", br(),
                               plotOutput("response")),
                      tabPanel("Discussion", 
                               br(), 
                               textOutput("instructionsOut1"),
                               br(), 
                               tags$div(id="fig0",class="shiny-image-output",style="width: 20% ;  height: 10%"),
                               br(),
                               textOutput("instructionsOut2"),
                               br(),
                               tags$div(id="fig1",class="shiny-image-output",style="width: 50% ;  height: 20%"),
                               br(),
                               textOutput("instructionsOut3"),
                               br(),
                               tags$div(id="fig2",class="shiny-image-output",style="width: 100% ;  height: 33%"),
                               br(),
                               textOutput("instructionsOut4"),
                               br(),
                               tags$div(id="fig3",class="shiny-image-output",style="width: 100% ;  height: 33%"),
                               br(),
                               tags$div(id="fig4",class="shiny-image-output",style="width: 100% ;  height: 33%"),
                               br(),
                               )
          ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$response <- renderPlot({
        # generate  plot from inputs in ui.R
        t_v <- seq(0, input$maxtime, 0.1)
        x_t <- x_t_fn(input$mass, input$damping, input$stiff, input$frequency, input$force, t_v)
        
        # draw the plot
        title <- paste0("Dynamic Response of a damped spring system\nmass=",input$mass," kg, damping coeff=", input$damping, " kg/s, \nstiffnesss coef=", input$stiff, " kg/s2, F0=",input$force," N, frequency=", input$frequency," radians/s")
        plot(t_v, x_t*10, type = "l",
             main = title,
             xlab = "Time, seconds",
             ylab = "Displacement, cm")
    })
    
    output$instructionsOut1 = renderText("Notes to develop the model implementation")
    output$instructionsOut2 = renderText("2. ")
    output$instructionsOut3 = renderText("3. ")
    output$instructionsOut4 = renderText("4. ")  
    
    filename0 <- normalizePath(file.path('./Images', paste('fig0.jpeg', sep='')))  
    filename1 <- normalizePath(file.path('./Images', paste('fig1.jpeg', sep='')))  
    filename2 <- normalizePath(file.path('./Images', paste('fig2.jpeg', sep='')))  
    filename0 <- normalizePath(file.path('./Images', paste('fig3.jpeg', sep='')))  
    filename1 <- normalizePath(file.path('./Images', paste('fig4.jpeg', sep='')))  
    
    output$fig0 = renderImage(list(src=filename0), deleteFile = FALSE)
    output$fig1 = renderImage(list(src=filename1), deleteFile = FALSE)
    output$fig2 = renderImage(list(src=filename2), deleteFile = FALSE)  
    output$fig3 = renderImage(list(src=filename0), deleteFile = FALSE)
    output$fig4 = renderImage(list(src=filename1), deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
