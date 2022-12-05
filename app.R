library(shiny)
project<- read.csv('project.csv')





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Project 3"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Project Capacity"=1, "Turbines"=2,"Year"=3,"Area Swept"=4, "Site of Project County"=5), 
                  selected = 1),
      
      # Slider input for number of bins
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      #option to show mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      
      #option to show sd
      checkboxInput("checkbox2", label="Display standard deviation", value=FALSE),
      
      #option to show median
      checkboxInput("checkbox3", label="Display median", value=FALSE),
      
      #option to show summary statistic
      checkboxInput("checkbox4", label="Display summary stats", value=FALSE),
      
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard deviation:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      p('Median:'),
      fluidRow(column(5, verbatimTextOutput("median"))),
      p('Summary Statistic :'),
      fluidRow(column(5, verbatimTextOutput("summary statistic"))),
      img(src="download.jpg", height = 250, width = 250),
      h1("This app shows the distribution of variables from a dataset that contains information about Wind Turbines, descriptive statistics are also included.")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar == 1){
      hist(project$Project.Capacity, breaks = input$bins, main='Distribution of Capacity of Project Sites',xlab='Capacity of Projects',col = 'purple', border = 'darkgrey')
    }
    
    if(input$selectvar == 2){
      hist(project$Project.Number_Turbines, breaks = input$bins, main='Distribution of Number of Turbines at Project Site',xlab='Number of Turbines ',col = 'purple', border = 'darkgrey')
      
    }
    if(input$selectvar == 3){
      hist(project$Year, breaks = input$bins, main='Distribution of Year Site was established ',xlab='Year of Site ',col = 'purple', border = 'darkgrey')
    }
    if(input$selectvar == 4){
      hist(project$Turbine.Swept_Area, breaks = input$bins, main='Distribution of Area Turbines Swept ',xlab='Area Swept ',col = 'purple', border = 'darkgrey')
    }
    if(input$selectvar == 5){
      counts <- table(project$Site.County)
      barplot(counts, main="Site Counties",
              xlab="Counties of Sites")    }
  })
  
  
  #Display mean if selected
  output$mean <- renderPrint({ 
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(project$Project.Capacity, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 2) {
      mean(project$Project.Number_Turbines, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 3) {
      mean(project$Year, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 4) {
      mean(project$Turbine.Swept_Area, na.rm=TRUE)}
    else if(input$checkbox1 == FALSE & input$selectvar == 5) {
      mean(project$Site.County, na.rm=FALSE)}
    
  })
  
  #Display sd if selected
  output$sd <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      sd(project$Project.Capacity, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 2){
      sd(project$Project.Number_Turbines, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 3){
      sd(project$Year, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 4){
      sd(project$Turbine.Swept_Area, na.rm=TRUE)}
    else if(input$checkbox2 == FALSE & input$selectvar == 5){
      sd(project$Site.County, na.rm=FALSE)}
  })
  
  #Display median if selected
  output$sd <- renderPrint({ 
    if(input$checkbox3 == TRUE & input$selectvar == 1){
      sd(project$Project.Capacity, na.rm=TRUE)}
    else if(input$checkbox3 == TRUE & input$selectvar == 2){
      sd(project$Project.Number_Turbines, na.rm=TRUE)}
    else if(input$checkbox3 == TRUE & input$selectvar == 3){
      sd(project$Year, na.rm=TRUE)}
    else if(input$checkbox3 == TRUE & input$selectvar == 4){
      sd(project$Turbine.Swept_Area, na.rm=TRUE)}
    else if(input$checkbox3 == FALSE & input$selectvar == 5){
      sd(project$Site.County, na.rm=FALSE)}
  })
  #Display summary statistics if selected
  output$sd <- renderPrint({ 
    if(input$checkbox4 == TRUE & input$selectvar == 1){
      sd(project$Project.Capacity, na.rm=TRUE)}
    else if(input$checkbox4 == TRUE & input$selectvar == 2){
      sd(project$Project.Number_Turbines, na.rm=TRUE)}
    else if(input$checkbox4 == TRUE & input$selectvar == 3){
      sd(project$Year, na.rm=TRUE)}
    else if(input$checkbox4 == TRUE & input$selectvar == 4){
      sd(project$Turbine.Swept_Area, na.rm=TRUE)}
    else if(input$checkbox4 == FALSE & input$selectvar == 5){
      sd(project$Site.County, na.rm=FALSE)}
  })
}









# Run the application 
shinyApp(ui = ui, server = server)