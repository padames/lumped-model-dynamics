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
library(here)

source(here::here("R", "compute_displacement.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(

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
                      value = 7),
          sliderInput("damping",
                      "Damping coefficient, kg/s",
                      min = 0,
                      max = 100,
                      value = 5),
          sliderInput("stiff",
                      "Stiffness coefficient, kg/s2",
                      min = 0,
                      max = 1,
                      value = 0.5),
          sliderInput("force",
                      "Max input force, N",
                      min = 0,
                      max = 10,
                      value = 4),
          sliderInput("frequency",
                      "Frequency, radian/s",
                      min = 0,
                      max = 25,
                      value = 2.7),
          sliderInput("maxtime",
                      "Response duration, minutes",
                      min = 0,
                      max = 199,
                      value = 5)
      ),
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Response", br(),
                               plotOutput("response")),
                      tabPanel("Discussion", 
                               br(), 
                               textOutput("instructionsOut0"),
                               br(), 
                               tags$div(id="fig0",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               textOutput("instructionsOut1"),
                               br(),
                               tags$div(id="fig1",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               textOutput("instructionsOut2"),
                               br(),
                               tags$div(id="fig2",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               textOutput("instructionsOut3"),
                               br(),
                               tags$div(id="fig3",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               textOutput("instructionsOut4"),
                               br(),
                               tags$div(id="fig4",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               textOutput("instructionsOut5"),
                               br(),
                               tags$div(id="fig5",class="shiny-image-output",style="width: 50% ;  height: 50%"),
                               br(),
                               )
          ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$response <- renderImage({
        # generate  plot from inputs in ui.R
        t_v <- seq(0, input$maxtime*60, 0.1) # the model needs seconds. 0.1 allow to capture detail when needed
        x_t <- x_t_fn(input$mass, input$damping, input$stiff, input$frequency, input$force, t_v)

        # A temp file to save the output. It will be deleted after renderImage
        # sends it, because deleteFile=TRUE.
        width  <- session$clientData$output_response_width
        height <- session$clientData$output_response_height
        
        outfile <- tempfile(fileext='.jpeg')
        
        # Generate a png
        png(outfile, width=width, height=height)
        # draw the plot
        title <- paste0("Dynamic Response of a damped spring system\nmass=",input$mass," kg, damping coeff=", input$damping, " kg/s, \nstiffnesss coef=", input$stiff, " kg/s2, F0=",input$force," N, frequency=", input$frequency," radians/s")
        plot(t_v, x_t/10, type = "l",
             main = title,
             xlab = "Time, seconds",
             ylab = "Displacement, cm")
        
        dev.off()
        
        # Return a list
        list(src = outfile,
             alt = "Dynamic response plot")
    }, deleteFile = TRUE)
    
    output$instructionsOut0 = renderText("1. Notes to develop the model implementation. Starting from the linear Ordinary Differential Equation (ODE)")
    output$instructionsOut1 = renderText("2. From time-domain ODE to Laplace-domain input-output formulation")
    output$instructionsOut2 = renderText("3. Obtaining the partial fraction expansion for X(s) is very calculation intense. This makes me feel young again!")
    output$instructionsOut3 = renderText("4. Now starting to put it all together. The initial conditions were used when transforming to Laplace.")  
    output$instructionsOut4 = renderText("5. The complementary solution always has the wave form. Below the development for the particular solution when the system si underdamped, also a wave.")
    output$instructionsOut5 = renderText("6. The final wave form for the particular solution when the denominator of the transfer function has two complex conjugate poles. Yahoo!")

    filename0 <- normalizePath(file.path('./Images', paste('fig0.jpeg', sep='')))  
    filename1 <- normalizePath(file.path('./Images', paste('fig1.jpeg', sep='')))  
    filename2 <- normalizePath(file.path('./Images', paste('fig2.jpeg', sep='')))  
    filename3 <- normalizePath(file.path('./Images', paste('fig3.jpeg', sep='')))  
    filename4 <- normalizePath(file.path('./Images', paste('fig4.jpeg', sep='')))  
    filename5 <- normalizePath(file.path('./Images', paste('fig5.jpeg', sep='')))  
    
    output$fig0 = renderImage(list(src=filename0), deleteFile = FALSE)
    output$fig1 = renderImage(list(src=filename1), deleteFile = FALSE)
    output$fig2 = renderImage(list(src=filename2), deleteFile = FALSE)  
    output$fig3 = renderImage(list(src=filename3), deleteFile = FALSE)
    output$fig4 = renderImage(list(src=filename4), deleteFile = FALSE)
    output$fig5 = renderImage(list(src=filename5), deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
