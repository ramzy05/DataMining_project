library(shiny)
library(shinythemes)

getDataset <- function() {
	dataset <- read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
	source('utils.r')
	return(pretaitement(dataset))
}
getTrainSet <- function(){
    set.seed(1)
    df <- getDataset()
    sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
    return (df[sample, ])
  test   <- df[!sample, ]
}
getTestSet <- function(){
    set.seed(1)
    df <- getDataset()
    sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
    return (df[!sample, ])
}

ui = fluidPage(
    theme = shinytheme("superhero"),

  	titlePanel("Credit Card"),

    # sidebarPanel(
    # 	# textInput("txt", "Text input:", "text here"),
    #   # sliderInput("slider", "Slider input:", 1, 100, 30),
    #   # actionButton("action", "Button"),
    #   # actionButton("action2", "Button2", class = "btn-primary")
    # ),
    mainPanel(
       tabsetPanel(type = "tabs",
          tabPanel("Table", tableOutput("table")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel(
            "Supervised Learning",
            selectInput("model_type", label = "Model", choices = c('tree','neuralnet','knn','svm')),
            tags$h4("Matrice de confusion"),
            tableOutput("conf_matrix"), 
            tags$h4("Details"),
            tableOutput("model_details")
          ),
      )
    )
  )

server = function(input, output, session) {
    updateProgress <- function(value = NULL, detail = NULL, progress) {
      # fonction de mise à jour de la barre de progression
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
  conf_matrix <- reactive({
	    source('utils.r')
    
     progress <- shiny::Progress$new()
          progress$set(message = "Traitement en cours", value = 0)
          # Close the progress when this reactive exits (even if there's an error)
          on.exit(progress$close())
          updateProgress(progress=progress)
          # Create a callback function to update progress.
          # Each time this is called:
          # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
          #   distance. If non-NULL, it will set the progress to that value.
          # - It also accepts optional detail text.

  
          model <- input$model_type
          out = supervisedLearnModelGenerator(getTrainSet(), getTestSet(), model_type = model)
          return(out)
    })

	 output$summary <- renderPrint({
      summary(getDataset())
		})
		
		output$table <- renderTable({
      dataset <- read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
    	dataset[1:16,]# 15 premieres lignes du datas	})
    })

		output$conf_matrix <- renderTable({
	    # source('utils.r')
      # model <- input$model_type
      # conf_matrix = supervisedLearnModelGenerator(getTrainSet(), getTestSet(), model_type = model)
      conf_matrix()
  	}, rownames=TRUE)
    
		output$model_details <- renderTable({
	    # source('utils.r')
      # model <- input$model_type
      # conf_matrix = supervisedLearnModelGenerator(getTrainSet(), getTestSet(), model_type = model)
      get_model_details(conf_matrix())
  	})
}
shinyApp(
    ui=ui,
    server = server
)

