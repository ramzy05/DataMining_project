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
            tableOutput("conf_matrix"), 
            tableOutput("model_details")
          ),
      )
    )
  )

server = function(input, output, session) {
	 output$summary <- renderPrint({
      summary(getDataset())
		})
		
		output$table <- renderTable({
      dataset <- read.csv(file = 'dataset_credit_card.csv',sep=',',header = T,na.strings = '?')
    	dataset[1:16,]# 15 premieres lignes du datas	})
    })

		output$conf_matrix <- renderTable({
	    source('utils.r')
      model <- input$model_type
      conf_matrix = supervisedLearnModelGenerator(getTrainSet(), getTestSet(), model_type = model)
      conf_matrix
  	}, rownames=TRUE)
    
		output$model_details <- renderTable({
	    source('utils.r')
      model <- input$model_type
      conf_matrix = supervisedLearnModelGenerator(getTrainSet(), getTestSet(), model_type = model)
      get_model_details(conf_matrix)
  	})
}
shinyApp(
    ui=ui,
    server = server
)

    