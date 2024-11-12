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
library(plotly)

source(here::here("R", "compute_displacement.R"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme =  shinythemes::shinytheme("flatly"),
    #  "shinythemes",
    titlePanel("Dynamic Response of a spring immersed in a fluid", windowTitle="Dynamic response simulation"),
    h4("Prepared by Pablo Adames on November 7, 2024"),
    sidebarLayout(
      sidebarPanel(
          sliderInput("mass_kg",
                      "Mass, kg",
                      min = 0.1,
                      max = 50,
                      value = 7),
          sliderInput("damping_kg_per_s",
                      "Damping coefficient, kg/s",
                      min = 0,
                      max = 100,
                      value = 8),
          sliderInput("stiff_kg_per_s2",
                      "Stiffness coefficient, kg/s2",
                      min = 0,
                      max = 5,
                      value = 0.5),
          sliderInput("force_newton",
                      "Max input force, N",
                      min = 0,
                      max = 30,
                      value = 4),
          sliderInput("frequency_rad_per_s",
                      "Frequency, radian/s",
                      min = 0,
                      max = 25,
                      value = 3),
          sliderInput("maxtime_minutes",
                      "Response duration, minutes",
                      min = 0,
                      max = 199,
                      value = 5)
      ),
      mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Response", br(),
                                plotlyOutput(outputId = "response")
                      ),
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
                      ),
                      tabPanel("Log", br(),
                               textOutput("logentry1"), br(),
                               textOutput("logentry2"), br(),
                      )
          ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$response <- renderPlotly({
        # generate the dynamic  response plot from inputs captured in the UI
        
        max_seconds_of_simulation <- input$maxtime_minutes * 60
        
        seconds_to_simulate <- seq(0, max_seconds_of_simulation, 0.1) # the model needs seconds. 0.1 allow to capture detail when needed

        input_parameters <- list(mass = input$mass_kg,
                                 damping = input$damping_kg_per_s,
                                 stiffness = input$stiff_kg_per_s2,
                                 frequency = input$frequency_rad_per_s,
                                 input_force = input$force_newton,
                                 vector_of_times = seconds_to_simulate)

        displacement_vector_meters <- displacement_fn(input_parameters)

        title <- paste0("Dynamic Response of a damped spring system initially at rest\nmass=",input$mass_kg,
                        " kg, damping coeff=", input$damping_kg_per_s, 
                        " kg/s, stiffnesss coef=", input$stiff_kg_per_s2, 
                        "kg/s2,\nexternal force=",input$force_newton,
                        " N, frequency=", input$frequency_rad_per_s," radians/s")

        data_to_plot <- data.frame(time=seconds_to_simulate, 
                                   displacement=displacement_vector_meters/10)
        
        fig <- plot_ly(x= data_to_plot$time, 
                       y=data_to_plot$displacement, 
                       type = "scatter",
                       mode = 'lines')
        
        max_time_minutes <- input$maxtime_minutes
        
        if (max_time_minutes >0 && max_time_minutes <= 5)
        {
          fig <- fig %>% layout(margin= list(t=100),
                                title = list(text = title, font=list(size=14, color="brown"), x=0.2, y = 0.83),
                                xaxis = list(title = "Time, seconds", dtick = 60, tickmode = "linear"),
                                
                                yaxis = list(title = "Displacement, cm"))
        }
        if (max_time_minutes > 5)
        {
          between_5_and_20 <- max_time_minutes <= 20
          between_20_and_300 <- max_time_minutes > 20 && max_time_minutes < 300
          ticks_by_minute <- ifelse( between_5_and_20, 1,ifelse(between_20_and_300, 10, 100))
          
          tick_values_minutes <- seq(0, max_time_minutes, ticks_by_minute)
          
          tick_values_text <- as.list(as.character(tick_values_minutes))
             
          tick_values_seconds <- as.list(tick_values_minutes * 60)
          
          fig <- fig %>% layout(margin= list(t=100),
                                title = list(text = title, font=list(size=14, color="brown"), x=0.2, y = 0.83),
                                xaxis = list(title = "Time, minutes", ticktext=tick_values_text, tickvals=tick_values_seconds, tickmode = "array"),
                                yaxis = list(title = "Displacement, cm"))
        }
        
        ggplotly(fig)
    })
    
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

    output$logentry1 = renderText("1. Nov 7, 2024: First version complete")
    output$logentry2 = renderText("2. Nov 10, 2024: Upgrade from native R plot to plotly library. Got zoom and print for free.")
}

# Run the application 
shinyApp(ui = ui, server = server)
