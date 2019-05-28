library(shiny)
library(dplyr)
library(shinydashboard)
library(readxl)

df_text<- read_excel("C:\\Users\\45054165\\Desktop\\Credit Card\\Card_Features.xlsx")

sidebar <- dashboardSidebar(
  sidebarMenu(
    
    dateInput(inputId = 'dob', max=Sys.Date()-(21*365), 
              label='Enter your Date of Birth', value =Sys.Date()-(25*365)
              #inputId = 'age', min = 21, max = 80,step = 5 ,value = 25,label = 'Enter Your Age'
    ),
    radioButtons(
      "citizen",
      "Are you a Singapore Citizen?",
      choices = c("Yes", "No"),
      selected = "Yes"
    ),
    numericInput(
      inputId = 'income', label = 'Enter your Annual Income (In SGD)', value = 30000,
      min=0, max=10000000
    ),
    radioButtons(
      "hsbc",
      "Are you an existing HSBC Customer?",
      choices = c("Yes", "No"),
      selected = "No"
    ),
    conditionalPanel(
      "input.hsbc== 'Yes'",
      radioButtons(
        "accounttype",
        "Choose HSBC account type",
        choices = c("Advance", "Premier", "Other"),
        selected = "Advance"
      )
    ),
    selectInput(
      'travel',
      'How often do you travel by plane in a year?',
      choices = c('<=6 trips','7-12 trips','>12 trips')
      
    ),
    checkboxGroupInput(
      inputId = 'purpose',label = 'Purpose for Credit Card?',
      choices =c('Dining','Groceries', 'Fuel','Online Purchase','Airport Lounge Access',
                 'Expedited Immigration', 'Air Miles', 'Local Entertainment')
    ),
    actionButton("goButton","Enter")
  )
)



 # A dashboard body with a row of infoBoxes and valueBoxes, and two rows of boxes
  body <- dashboardBody(
    
    # infoBoxes
    fluidRow(),
    fluidRow(
      br(),
      br(),
      br(),
      infoBox(
        "Recommended Credit Card #1 ", 
        plotOutput("im1", height = 190), 
        h4(textOutput('p1')),
      #  verbatimTextOutput('p1'),
        icon = icon("credit-card"),
        href = textOutput('l1'),
        color = "blue"
      ),
      infoBox(
        "Recommended Credit Card #2 ", plotOutput("im2", height = 190),
        h4(textOutput('p2')),
      #  verbatimTextOutput('p2'),
        icon = icon("credit-card"), href =textOutput('l2'),
        color = "blue"
      ),
      infoBox(
        "Recommended Credit Card #3 ", plotOutput("im3", height = 190), 
        h4(textOutput('p3')),
        #verbatimTextOutput('p3'),
        icon = icon("credit-card"), href = textOutput('l3'),
        color = "blue"
      )
      
        ),
    
    
  
    
    # Boxes with solid color, using `background`
    fluidRow(
      br(),
      # Box with textOutput
      valueBoxOutput("vbox1"),
      valueBoxOutput("vbox2"),
      valueBoxOutput("vbox3")
     
     
     # box(
     #   width = 4,
     #   background = "light-blue",
     #   p("This is content. The background color is set to light-blue")
     # )
    ),
    fluidRow(
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      
      conditionalPanel("input.citizen == 'No' & input.income <40000", 
                       wellPanel(tags$i("*Since you do not meet the minimum income requirement, 
                                                      a minimum Fixed Deposit Collateral of S$10,000 is required."))),
      conditionalPanel("input.citizen == 'Yes' & input.income <30000", 
                       wellPanel(tags$i("*Since you do not meet the minimum income requirement, 
                                               a minimum Fixed Deposit Collateral of S$10,000 is required.")))
      
      
    )
  )
  
  server <- function(input, output) {
    
    purpose <- eventReactive(input$goButton,{
      input$purpose
    })
    
 
    
    
    pos<- eventReactive(input$goButton,{
      
      df_card_master<- read.csv(file = 'C:\\Users\\45054165\\Desktop\\Credit Card\\card_master.csv',
                                header = TRUE,stringsAsFactors = FALSE, na.strings = c(NA,""))
      
      df_text<- read_excel("C:\\Users\\45054165\\Desktop\\Credit Card\\Card_Features.xlsx")
      
      #  count<- c(,0,0,0,0)
      count<- data.frame(card=c('VISA Platinum', 'Advance','Revolution','VISA Infinite','HSBC Premier')
                         ,match=c(0,0,0,0,0),
                         user_card_priority=c(3,3,3,3,5),
                         air_miles_priority=c(3,3,3,3,3),
                         general_priority = c(1,2,3,4,4)
      )
      
      
      #update for advance
      count[2,3]<-  ifelse(input$hsbc=='Yes' & input$accounttype=='Advance',1,count[2,3])
      count[3,3]<-  ifelse(input$hsbc=='Yes' & input$accounttype=='Advance',2,count[3,3])
      count[4,3]<-  ifelse(input$hsbc=='Yes' & input$accounttype=='Advance',2,count[3,3])
      
      #update for premier
      count[5,3] <- ifelse(input$hsbc=='Yes' & input$accounttype=='Premier',1,count[5,3])
      
      #update air miles priority
      
      count[3,4]<- ifelse(input$travel=='7-12 trips',1,count[3,4])
      count[4,4] <- ifelse(input$travel=='7-12 trips',2,count[4,4])
      count[5,4] <- ifelse(input$travel=='7-12 trips',2,count[5,4])
      
      
      count[3,4]<- ifelse(input$travel=='>12 trips',2,count[3,4])
      count[4,4] <- ifelse(input$travel=='>12 trips',1,count[4,4])
      count[5,4] <- ifelse(input$travel=='>12 trips',1,count[5,4])
      
      count[1,4]<- ifelse(input$travel=='<=6 trips',1,count[1,4])
      count[2,4] <- ifelse(input$travel=='<=6 trips',1,count[2,4])
      #  count[5,4] <- ifelse(input$travel=='<=6 trips',1,count[5,4])
      
      # choices =c('Dining','Groceries', 'Fuel','Online Purchase','Airport Lounge Access',
      #          'Expedited Immigration', 'Air Miles', 'Local Entertainment')
      
      for (i in 1:length(purpose()))
      {
        for (j in 1:5 )
        {
          count[j,2]<- count[j,2] + as.numeric(tolower(purpose()[i]) %in% tolower(df_card_master[,j]))
        }
      }
      
      count%>%arrange(user_card_priority,air_miles_priority,-match,-general_priority) ->df
      if(!(input$hsbc=='Yes' & input$accounttype=='Premier')){
        df<- df[-which(tolower(df[,1])=='hsbc premier'),]
      }
      #df%>%filter
      df[,1]
    })
    
    image1<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[1]),
              "visa platinum" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_platinum.jpeg',
              "advance" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\advance.jpeg',
              "revolution" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\revolution.jpeg',
              "visa infinite" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_infinite.jpeg',
              "hsbc premier" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\premier.jpeg'
      )
    })
    
    image2<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[2]),
              "visa platinum" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_platinum.jpeg',
              "advance" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\advance.jpeg',
              "revolution" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\revolution.jpeg',
              "visa infinite" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_infinite.jpeg',
              "hsbc premier" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\premier.jpeg'
      )
    })
    
    image3<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[3]),
              "visa platinum" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_platinum.jpeg',
              "advance" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\advance.jpeg',
              "revolution" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\revolution.jpeg',
              "visa infinite" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\visa_infinite.jpeg',
              "hsbc premier" = 'C:\\Users\\45054165\\Desktop\\Credit Card\\Images\\premier.jpeg'
      )
    })
    
    link1<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[1]),
              "visa platinum" = 'https://www.hsbc.com.sg/1/2//campaigns/chl_vpccard/',
              "advance" = 'https://www.hsbc.com.sg/credit-cards/products/advance/',
              "revolution" = 'https://www.hsbc.com.sg/1/2//campaigns/chl_revo',
              "visa infinite" = 'https://www.hsbc.com.sg/1/2//campaigns/visainfinite',
              "hsbc premier" = 'https://www.hsbc.com.sg/1/2//personal/cards/hsbc-premier-mastercard/'
      )
    })
    
    link2<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[2]),
              "visa platinum" = 'www.hsbc.com.sg/1/2//campaigns/chl_vpccard/',
              "advance" = 'www.hsbc.com.sg/credit-cards/products/advance/',
              "revolution" = 'www.hsbc.com.sg/1/2//campaigns/chl_revo',
              "visa infinite" = 'www.hsbc.com.sg/1/2//campaigns/visainfinite',
              "hsbc premier" = 'www.hsbc.com.sg/1/2//personal/cards/hsbc-premier-mastercard/'
      )
    })
    
    link3<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[3]),
              "visa platinum" = 'www.hsbc.com.sg/1/2//campaigns/chl_vpccard/',
              "advance" = 'www.hsbc.com.sg/credit-cards/products/advance/',
              "revolution" = 'www.hsbc.com.sg/1/2//campaigns/chl_revo',
              "visa infinite" = 'www.hsbc.com.sg/1/2//campaigns/visainfinite',
              "hsbc premier" = 'www.hsbc.com.sg/1/2//personal/cards/hsbc-premier-mastercard/'
      )
    })
    
    
    text1<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[1]),
              "visa platinum" = df_text[,1],
              "advance" = df_text[,2],
              "revolution" = df_text[,3],
              "visa infinite" = df_text[,4],
              "hsbc premier" = df_text[,5]
      )
    })
    
    
    text2<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[2]),
              "visa platinum" = df_text[,1],
              "advance" = df_text[,2],
              "revolution" = df_text[,3],
              "visa infinite" = df_text[,4],
              "hsbc premier" = df_text[,5]
      )
    })
    
    
    text3<- eventReactive(input$goButton,{
      
      switch (tolower(pos()[3]),
              "visa platinum" = df_text[,1],
              "advance" = df_text[,2],
              "revolution" = df_text[,3],
              "visa infinite" = df_text[,4],
              "hsbc premier" = df_text[,5]
      )
    })
    
    output$vbox1 <- renderValueBox({
      valueBox( "Features", icon = icon("credit-card"),
                tags$ol(lapply(1:nrow(as.data.frame(text1())), function(x) {
                  if(!is.na(as.data.frame(text1())[x,1]))
                  return(tags$li(as.data.frame(text1())[x,1]))
                })
                ), color = "blue"
      )
      
    })
    
    output$vbox2 <- renderValueBox({
      valueBox( "Features", icon = icon("credit-card"),
                tags$ol(lapply(1:nrow(as.data.frame(text2())), function(x) {
                  if(!is.na(as.data.frame(text2())[x,1]))
                  return(tags$li(as.data.frame(text2())[x,1]))
                })
                ), color = "blue"
      )
      
    })
    
    output$vbox3 <- renderValueBox({
      valueBox( "Features", icon = icon("credit-card"),
                tags$ol(lapply(1:nrow(as.data.frame(text3())), function(x) {
                  if(!is.na(as.data.frame(text3())[x,1]))
                  return(tags$li(as.data.frame(text3())[x,1]))
                })
                ), color = "blue"
      )
      
    })
    
    
    
        output$im1<- renderImage({
      list(src=image1(),height=194,width=300)
      
    },deleteFile = FALSE)
    output$im2<- renderImage({
      list(src=image2(),height=194,width=300)
      
    },deleteFile = FALSE)
    output$im3<- renderImage({
      list(src=image3(),height=194,width=300)
      
    },deleteFile = FALSE)
    output$p1 <- renderText(pos()[1])
    output$p2<- renderText(pos()[2])
    output$p3<- renderText(pos()[3])
    
    output$l1 <- renderText(link1())
    output$l2<- renderText(link2())
    output$l3<- renderText(link3())
    
    #output$txt1<- renderInfoBox()
    
}
 #   renderbos
  
  shinyApp(
    ui = dashboardPage(
      skin = 'blue',
      dashboardHeader(
        title = "Credit Card Recommender(Singapore)",
        titleWidth = 400
      ),
      sidebar,
      body
    ),
    server = server
  )

