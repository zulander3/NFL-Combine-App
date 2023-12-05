library(ggplot2)
library(shiny)

ui <- fluidPage(

    titlePanel("NFL Combine Measurables"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "XVar",
                        label = "Predictor Variable (x)",
                        choices = list("Wt", "Dash", "Vertical", "Bench", "Broad_Jump", "Cone", "Shuttle")),
            
            selectInput(inputId = "YVar",
                        label = "Response variable (y)",
                        choices = list("Wt", "Dash", "Vertical", "Bench", "Broad_Jump", "Cone", "Shuttle")),
            checkboxInput(inputId = "reg",
                          label = "Fit Regression Line",
                          value = FALSE
                          )
        ),

        mainPanel(
           plotOutput("ScatterPlot")
        )
    )
)


server <- function(input, output) {
    dat <- reactive({
      file <- "2022_combine.csv"
      read.csv(file)
    })
    output$ScatterPlot <- renderPlot({
        var <- dat()[ ,c(input$XVar,input$YVar)]
        p1 <- ggplot(data = var, aes(x=var[,1],y=var[,2]))+
          geom_point()+
          labs(x=colnames(var)[1], y=colnames(var)[2],
               title = paste(input$XVar, "Vs", input$YVar))
        
        coefs <- lm(var[,2] ~ var[,1], data = var)$coefficients
        
        
        p2 <- {if (input$reg) p1 + 
                 geom_abline(slope = coefs[2],
                             intercept = coefs[1]) 
               else p1}
        
        print(p2)    
    
    })
}

shinyApp(ui = ui, server = server)
