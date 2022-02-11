library(randomcoloR)
library(tidyverse)
library(truncnorm)
library(shiny)
library(ggthemes)

css <- HTML(" body {
    background-color: #d5e4eb;
}
    #sidebar {
            background-color: #76a5c2;
    }
        
    body, label, input, button, select { 
          font-family: 'Arial';
          color: #000000
    }
    
    .tabbable > .nav > li > a                  {background-color: #76a5c2;  color:black}
    
    .tabbable > .nav > li[class=active] > a {
           background-color: #538aa3;
           font-family:'Arial';
           color: #000000}
    

")

ui <- fluidPage(
  tags$head(tags$style(css)),
  theme = shinythemes::shinytheme("cerulean"),
  titlePanel(title=div(img(src="Captura.png"))),
  tabsetPanel(
    tabPanel(icon("home"),
             fluidRow(
                      column(
                        
                        br(),
                        p("Probabilistic Spreadsheet is a web application to generate and perform calculations with 
                        uncertain quantities based on probability distributions. It uses Monte Carlo sampling to generate its results. 
                        Inspired by Caladis.",
                          style="text-align:justify;color:black;background-color:#76a5c2;padding:15px;border-radius:10px;font-size:large"),
                        br(),
                        h2(strong("How it works"),style="text-align:justify;color:black"),
                        p(strong("1."),"Open every tab from left to right waiting in each of them for the demo to load. 
                          (make sure that the demo is properly loaded)",
                          style="text-align:justify;color:black;background-color:#76a5c2;padding:15px;border-radius:10px"),
                        p(strong("2."),'Go back to the "Add new variable" tab, now you can add any distribution that you wish.',
                          strong("Be careful: change the name each time or the app will crash!"),
                          style="text-align:justify;color:black;background-color:#76a5c2;padding:15px;border-radius:10px"),
                        p(strong("3."),'Go to the "Calculator" tab and perform any calculations with your newly created distributions.',
                          strong("Be careful: Change the name or leave it empty."),
                          style="text-align:justify;color:black;background-color:#76a5c2;padding:15px;border-radius:10px"),
                        p(strong("4."),'Congratulations, you have made your first calculation! Now you can go to the "Summaries" and "Probabilities" 
                          tabs to find out more about your newly created distributions and calculations.',
                          style="text-align:justify;color:black;background-color:#76a5c2;padding:15px;border-radius:10px"),
                        
                        width=8)
                      )
             ),
    tabPanel("Add new variable",
             sidebarLayout(
               sidebarPanel(id="sidebar",
                 textInput("name", "Name:",value = "Example_Normal"),
                 selectInput("dist",
                             "Probability Distribution",
                             choices = list(
                               "Normal" = "normal",
                               "Truncated Normal" = "truncated_normal",
                               "Log-normal" = "log_normal",
                               "Binomial" = "binomial",
                               "Gamma" = "gamma",
                               "X²" = "chisq",
                               "Student's T" = "st",
                               "Cauchy" = "cauchy",
                               "Generalized Pareto" = "gp"),
                             selected = "normal"),
                 
                 uiOutput("params"),
                 actionButton("add", label = "Add")
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(style = "overflow-y:scroll; max-height: 2000px; position:relative;",
                         uiOutput("plots")
                         
               )
             )),
    
    tabPanel("Calculator",
             verticalLayout(
               wellPanel(id="sidebar",
                 textInput("calc", "Enter a name",
                           value="Example"),
                 textInput("expr", "Enter an Expresion",
                           value="#Example_Normal + 5"),
                 p(
                   "Use the '#' symbol to denote variables created previously, e.g. #Var1 + #Var2 ",
                 ),
                 actionButton("addexpr", label = "Calculate"))
             ),
             mainPanel(
               uiOutput("calcplots")
             )
    ),
    
    tabPanel("Summaries",
             fluidRow(
               column(8, h3("Distribution's Summaries", align="center"),
                      fluidRow(
                        column(5, h4("Calculations", align="center")),
                        column(7, h4("Variables", align="center"))
                      ))),
             mainPanel(style = "overflow-y:scroll; max-height: 2000px; position:relative;",
                       splitLayout(uiOutput("summarycalc"),
                                   uiOutput("summaryvar"))
             )),
    
    tabPanel("Probabilities",
             sidebarPanel(id="sidebar",
               uiOutput("choices"),
               selectInput("threshold",
                           "Find probability:",
                           choices = list("Lower" = "lower",
                                          "Higher" = "higher")),
               uiOutput("prob_params")
               ),
             mainPanel(
               plotOutput("dist"),
               textOutput("prob")
               )
             ),
    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  observeEvent(input$add, {
    showNotification(paste("Distribution", input$name, "created"), type="message")
  })
  
  almacen <- reactiveValues()
  almacen_calc <- reactiveValues()
  
  output$choices <- renderUI({
    
    selectInput("choice",
                "Select distribution:",
                choices= names(almacen_calc))

  })
  
  
  output$prob_params <- renderUI({
    
    if (input$threshold == "lower" | input$threshold =="higher")
      
      choice <- input$choice

          sliderInput("tail",
                  label = "Threshold",
                  min = round(min(almacen_calc[[choice]]), 2),
                  max = round(max(almacen_calc[[choice]]), 2),
                  value = round(median(almacen_calc[[choice]]), 2))
          
  })
  
  output$prob <- renderText({
    
    choice <- input$choice
    almacen_filtered <- almacen_calc[[choice]]
    
    
    probability <- length(almacen_filtered[almacen_filtered>input$tail])/1000
    
    if (input$threshold=="higher") {
      
      print(paste("Probability of", input$choice,
                  "being higher than", input$tail, "is", probability, "%"))
      
    } else if (input$threshold=="lower") {
      
      print(paste("Probability of", input$choice,
                  "being lower than", input$tail, "is", 100-probability, "%"))
      
    }

    
  })
  
  output$params <- renderUI({
    
    if (input$dist == "normal")
      # TagList crea un hermano dentro de un renderUI
      tagList(
        numericInput("mean",
                     label = "Mean",
                     value = 0),
        numericInput("sd",
                     label = "Standard Deviation",
                     value = 1))
    
    else if (input$dist == "log_normal")
      tagList(
        numericInput("mean",
                     label = "Mean (log)",
                     value = 0),
        numericInput("max",
                     label = "Standard Deviation (log)",
                     value = 1))
    
    else if (input$dist == "binomial")
      tagList(
        numericInput("size",
                     label = "Number of trials",
                     value = 10),
        numericInput("prob",
                     label = "Probability of success",
                     min = 0,
                     max = 1,
                     value = 0.5))
    
    else if (input$dist == "gamma")
      tagList(
        numericInput("shape",
                     label = "Shape",
                     value = 1),
        numericInput("rate",
                     label = "Scale",
                     value = 1))
    
    else if (input$dist == "chisq")
      tagList(
        numericInput("degrees",
                     label = "Degress of freedom",
                     value = 1),
        numericInput("ncp",
                     label = "Non-centrality Parameter ",
                     value = 1))
    
    else if (input$dist == "truncated_normal")
      tagList(
        numericInput("minval",
                     label = "Lower Bounds",
                     # A mejorar
                     value = -Inf),
        numericInput("maxval",
                     label = "Upper Bounds",
                     # A mejorar
                     value = Inf),
        numericInput("mean",
                     label = "Mean",
                     value = 0),
        numericInput("sd",
                     label = "Standard Deviation",
                     value = 1))
    
    else if (input$dist == "st")
      tagList(
        numericInput("degrees",
                     label = "Degress of freedom",
                     value = 1),
        numericInput("ncp",
                     label = "Non-centrality Parameter",
                     value = 0))
    
    else if (input$dist == "cauchy")
      tagList(
        numericInput("loc",
                           label = "Location",
                           value = 0),
        numericInput("scale",
                           label = "Scale",
                           value = 1))
    
    else if (input$dist == "gp")
      tagList(
        numericInput("loc",
                           label = "Location",
                           value = 0),
        numericInput("scale",
                           label = "Scale",
                           value = 1),
        numericInput("shape",
                     label = "Shape",
                     value=0))
    
  })
  
  
  output$plots <- renderUI({
    
    plot_output_list <- lapply(0:(input$add), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 500, width = 1000)
      
    })
    
    do.call(tagList, plot_output_list)
  })
  
  output$summarycalc <- renderUI({
    
    summary_output_list <- lapply(0:(input$addexpr), function(i) {
      summarycalcname <- paste("summarycalc", i, sep="")
      verbatimTextOutput(summarycalcname)
      
    })
    
    do.call(tagList, summary_output_list)
  })
  
  output$summaryvar <- renderUI({
    
    summaryvar_output_list <- lapply(0:(input$add), function(i) {
      summaryvarname <- paste("summaryvar", i, sep="")
      verbatimTextOutput(summaryvarname)
      
    })
    
    do.call(tagList, summaryvar_output_list)
  })
  
  
  output$calcplots <- renderUI({
    
    calcplot_output_list <- lapply(0:(input$addexpr), function(i) {
      calcplotname <- paste("calcplot", i, sep="")
      plotOutput(calcplotname, height = 500, width = 1700)
      
    })
    
    do.call(tagList, calcplot_output_list)
  })
  
  output$dist <- renderPlot({
    
    chosen <- input$choice
    isolate(print(tibble(value = almacen_calc[[chosen]]) %>% 

                    ggplot(aes(value, y = (..count..)/sum(..count..)))+ 
                    geom_histogram(fill=randomColor(), color='black')+
                    labs(y='Frequency')+
                    theme_economist() + scale_colour_economist() +
                    ggtitle(paste('Distribution'))))

  })
  
  
  
  
  
  max_plots <- 100
  
  for (i in 0:max_plots) {
    
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      summaryvarname <- paste("summaryvar", my_i, sep="")
      summarycalcname <- paste("summarycalc", my_i, sep="")
      calcname <- paste("calcplot", my_i, sep="")
      
      
      output[[plotname]] <- renderPlot({
        
        distribucion <- isolate(input$dist)
        
        if (distribucion == "normal"){
          
          mean <- isolate(input$mean)
          nombre <- isolate(input$name)
          sd <- isolate(input$sd)
          req(mean, sd)
          
          
          almacen[[nombre]] <- rnorm(100000, mean, sd)
          
        } else if (distribucion =="log_normal"){
          
          mean <- isolate(input$mean)
          nombre <- isolate(input$name)
          sd <- isolate(input$max)
          req(mean, sd)
          
          almacen[[nombre]] <- rlnorm(100000, mean, sd)
          
        } else if (distribucion =="binomial"){
          
          size <- isolate(input$size)
          nombre <- isolate(input$name)
          probability <- isolate(input$prob)
          req(size, probability)
          
          almacen[[nombre]] <- rbinom(100000, size, probability)
          
        } else if (distribucion =="truncated_normal"){
          
          min <- isolate(input$minval)
          max <- isolate(input$maxval)
          mean <- isolate(input$mean)
          sd <- isolate(input$sd)
          nombre <- isolate(input$name)
          req(min, max, mean, sd)
          
          almacen[[nombre]] <- rtruncnorm(100000, min, max, mean, sd)
          
        } else if (distribucion =="gamma"){
          
          shape <- isolate(input$shape)
          rate <- isolate(input$rate)
          nombre <- isolate(input$name)
          req(shape, rate)
          
          almacen[[nombre]] <- rgamma(100000, shape, rate)
          
        } else if (distribucion =="chisq"){
          
          degrees <- isolate(input$degrees)
          ncp <- isolate(input$ncp)
          
          nombre <- isolate(input$name)
          req(degrees, ncp)
          
          almacen[[nombre]] <- rchisq(100000, degrees, ncp)
          
        } else if (distribucion =="st"){
          
          degrees <- isolate(input$degrees)
          ncp <- isolate(input$ncp)
          
          nombre <- isolate(input$name)
          req(degrees, ncp)
          
          almacen[[nombre]] <- rt(100000, degrees, ncp)
          
        } else if (distribucion =="cauchy"){
          
          loc <- isolate(input$loc)
          scale <- isolate(input$scale)
          
          nombre <- isolate(input$name)
          req(loc,scale)
          
          almacen[[nombre]] <- rcauchy(100000,loc,scale)
          
        } else if (distribucion =="gp"){
          
          loc <- isolate(input$loc)
          scale <- isolate(input$scale)
          shape <- isolate(input$shape)
          
          nombre <- isolate(input$name)
          req(loc,scale,shape)
          
          almacen[[nombre]] <- DescTools::rGenPareto(100000,loc,scale,shape)
          
          
          
        }
        
        print(tibble(value = almacen[[nombre]]) %>% 
                ggplot(aes(value, y = (..count..)/sum(..count..)))+ 
                geom_histogram(fill=randomColor(), color='black')+
                labs(y='Frequency')+
                theme_economist() + scale_colour_economist() +
                ggtitle(paste('Distribution',nombre)))
        
      })
      
      output[[summarycalcname]] <- renderPrint({
        
        
        print(paste("Distribution", isolate(input$calc)))
        summary(almacen_calc[[isolate(input$calc)]])
        
      })
      
      output[[summaryvarname]] <- renderPrint({
        
        
        print(paste("Distribution", isolate(input$name)))
        summary(almacen[[isolate(input$name)]])
        
      })
      
      
      output[[calcname]] <- renderPlot({
        
        nombre_calculo <- if_else(isolate(input$calc)=="", isolate(input$expr), isolate(input$calc))
        
        expresion <- isolate(input$expr)
        expresion <- str_replace_all(expresion, "#", "almacen$")
        
        almacen_calc[[nombre_calculo]] <- isolate(eval(parse(text=expresion)))
        
        isolate(print(tibble(value = almacen_calc[[nombre_calculo]]) %>% 
                        ggplot(aes(value, y = (..count..)/sum(..count..)))+ 
                        geom_histogram(fill=randomColor(), color='black')+
                        theme_economist() + scale_colour_economist() +
                        labs(y='Frequency')+
                        ggtitle(paste('Distribution', nombre_calculo))))
        
      })
      
      
      
    })
  }
  
}

shinyApp(ui=ui, server=server)

