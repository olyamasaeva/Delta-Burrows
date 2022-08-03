ui <- fluidPage(  
  actionButton("go", "Go"),
  checkboxGroupInput("checkGroup1", label = h3("This is a Checkbox group"), 
                     choices = list("Fedor Dostoevsky" = "Fedor Dostoevsky", "Michail Dostoevsky" = "Michail Dostoevsky", 
                                    "Polonsky" = "Polonsky","Shchebalsky" = "Shchebalsky","Pobedonostsev" = "Pobedonostsev",
                                    "Strahov" = "Strahov","Grigoriev" = "Grigoriev","Meshchersky" = "Meshchersky"),
                     selected = 1),
  fluidRow(column(3, verbatimTextOutput("text_choice")))
) 


server <- function(input, output){
  observe({
  if(input$go > 0 ){
    stopApp(input$checkGroup1)
  }
  })
} 

