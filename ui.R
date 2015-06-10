##########################################################
####### ASMDA PROJECT APP FOR VISUALIZING TEXTUAL DATA RESULTS
##########################################################
library(shiny)


shinyUI(fixedPage(
  img(alt='UN', src='http://www.agenciadenoticias.unal.edu.co/uploads/pics/unal_full_85.jpg', width=200, height=150, align="right"),
  br(),
  br(),
  h1("GUN DEATHS CORPUS DATA", style = "font-family: 'Palatino';color: #0C5168; font-size:55px"),
  br(),
  h3("Textual Analysis about deaths in the USA",style = "font-family: 'Signika Negative';color: #510C68; font-size:35px"),
  h5("Welcome to our textual analysis tool! Choose one topic and get the insights about it",
     style = "font-family: 'Signika Negative';color: #510C68; font-size:20px"),
  headerPanel("",windowTitle="Gun Deaths App"),
  #h3("Textual analysis using a latent regression model", style = "font-family: 'Palatino';color: #510C68; font-size:35px"),  
  #sidebarLayout(
    sidebarPanel(width=3,
                 h4("1. Topics",style = "font-family: 'Palatino'; font-size:30px"),
                 selectInput("them.axis", " ", c("Family crimes", "Trials", "Evidences and tests results", "Automobile deaths", "Urban crimes", "Reported cases")),
                 h5("Characteristic terms",style = "font-family: 'Palatino'; font-size:15px"),
                 tableOutput("Terms_x_axis"),
                 #verbatimTextOutput("Terms_x_axis"),
                 h5("2. Document ID",style = "font-family: 'Palatino'; font-size:30px"),
                 numericInput("artid", "", 1, min=1, max=1000),
                 actionButton("goButton", "Go!"),
                 h6("Get information in the Graph and Text tabs",style = "font-family: 'Palatino'; font-size:15px")
                 ),#sidebarpanel
    mainPanel(
      tabsetPanel(
        tabPanel("List of textual units", h4("Choose a document and enter the ID below",style = "font-family: 'Signika Negative'; font-size:20px"), tableOutput("Arts_x_axis")),
        tabPanel("Projection Graph", h4("Radar of the document throughout the topics",style = "font-family: 'Signika Negative'; font-size:20px"),tableOutput("art_title"),plotOutput("proj_graph", height=700, width=700)),
        tabPanel("Text", h4("Complete document",style = "font-family: 'Signika Negative'; font-size:20px"),verbatimTextOutput("text"))
      )
    )
   # )#sidebarlayout
)#Fluidpage
)#shinyUI