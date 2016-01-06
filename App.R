library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)
library(dplyr)

setwd("C:/Users/193344/Desktop/tracker gui")
ARMASTER <- read.csv("//knx1fs01/ED Reporting/Lowhorn Big Data/Golden Rule Data/ARMASTER.csv")

ar <- select(ARMASTER,A.R,desk,manager)
ar <- rename(ar,Desk=desk)
ar <- rename(ar,Manager=manager)

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


fields <- c("Desk","Program","File #", "Set Up Date","Tier","Due Date","Payment Method","Payment Amount")


shinyApp(
  ui = fluidPage(shinyjs::useShinyjs(), theme=shinytheme("readable"),
               
                   
                 
                 tags$head(
                   tags$style(HTML("
                                   @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                                   
                                   h1 {
                                   font-family: 'Lobster', cursive;
                                   font-weight: 500;
                                   line-height: 1.1;
                                   color: #191970;
                                   }
                                   
                                   "))
                   ),            
                 
                 headerPanel("AM Tracker App"),
                 tabsetPanel( 
                   tabPanel("User Input",div(id = "form",
                            column(4),column(4,
                                                          
                                                          numericInput("Desk", "Desk", ""),
                                                          numericInput("File #","File #",""),
                                                          dateInput("Set Up Date","Set Up Date",value=Sys.Date()),
                                                          selectInput("Program","Program",choices=c("15%","FIS","SIF","BIF")),
                                                          selectInput("Tier","Tier",choices=c("Extreme","High","Medium","Low")),
                                                          dateInput("Due Date","Due Date",value=Sys.Date()),
                                                          selectInput("Payment Method","Payment Method",choices=c("Credit Card","Debit Card","Check","Money Order","WU","Mail In")),
                                                          numericInput("Payment Amount","Payment Amount",""),
                                                          
                                                          actionButton("submit", "Submit",icon=icon("eye-open", lib = "glyphicon")), 
                                                          tags$style(type='text/css', "#submit { vertical-align: middle; height: 50px; width: 69%; font-size: 30px;  background-color: LightGray; border-color: black; color:blue}")
                   )),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h3("Thanks, your account was submitted successfully!"),
                     actionLink("submit_another", "Submit another account")
                   ))),
                 
                   tabPanel("Daily Tracker",
                            fluidRow (
                              
                              
                              dataTableOutput("table1"
                              ))),
                   tabPanel("Daily Duplicates",
                            
                            dataTableOutput("dupes")
                   ),
                   tabPanel("Master Tracker", fluidRow(column(1),
                                                      column(5,
                                                             selectInput("Manager",
                                                                         "Manager Select",
                                                                         choices=levels(MasterTracker$Manager),
                                                                         multiple=T,
                                                                         selected=levels(MasterTracker$Manager),
                                                                         selectize=T,
                                                                         width=1000)),
                                                      column(5,
                                                             selectInput('Month',"Month Select",
                                                                         choices=c("August 2015","September 2015"),
                                                                         selected="September 2015",
                                                                         selectize=T,
                                                                         multiple=T,
                                                                         width=1000)
                                                             
                                                             ),
                                                      column(1)),
                            DT::dataTableOutput("MasterTracker")),
                   tabPanel("Office Tracker", fluidRow(column(1),
                                                       column(5,
                                                              selectInput("MGR",
                                                                          "Manager Select",
                                                                          choices=levels(Budgets$Manager),
                                                                          multiple=T,
                                                                          selected=levels(Budgets$Manager),
                                                                          selectize=T,
                                                                          width=1000)),
                                                      
                                                       column(5,selectInput('MTH',"Month Select",
                                                                           choices=c("September 2015","August 2015"),
                                                                           selected="September 2015",
                                                                           selectize=T,
                                                                           multiple=T,
                                                                           width=1000)),
                                                       column(1),
                            DT::dataTableOutput("OfficeTracker")
                            
                            
                   )),
                   tabPanel("Historical Duplicates",
                            DT::dataTableOutput("MTH")
                   )
                   
                   
                   
                 )),
  server = function(input, output, session) {
 
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })    
    
    output$table1 <- renderDataTable({
      
    input$submit
     loadData()
      
      responses$Desk <- unlist(as.integer(as.character(responses$Desk)))
      
      
      responses <- left_join(responses,ar,by="Desk")
     
      responses$"Set Up Date" <- unlist(as.numeric(as.character(responses$"Set Up Date")))
      responses$"Set Up Date" <- as.Date(responses$"Set Up Date", origin = "1970-01-01")
      responses$"Due Date" <- as.numeric(as.character((responses$"Due Date")))
      responses$"Due Date" <- as.Date(responses$"Due Date", origin = "1970-01-01")
      
      
      responses
      
      
    },options = list(lengthMenu = c(10, 50, 100, 3000), pageLength = 3000))
    
    
    output$dupes <- renderDataTable({
      
      input$submit
      loadData()
      
      responses$Desk <- unlist(as.integer(as.character(responses$Desk)))
      
      responses$"Set Up Date" <- as.numeric(as.character(responses$"Set Up Date"))
      responses$"Set Up Date" <- as.Date(responses$"Set Up Date", origin = "1970-01-01")
      responses$"Due Date" <- as.numeric(as.character(responses$"Due Date"))
      responses$"Due Date" <- as.Date(responses$"Due Date", origin = "1970-01-01")
      
      responses2 <- responses[duplicated(responses[,3]),]
      responses2 <- responses2$"File #"
      response <- responses[responses$"File #" %in% responses2,]
      response <- left_join(response,ar,by="Desk")
      #response$"Set Up Date" <- as.numeric(as.character(response$"Set Up Date"))
      response$"Set Up Date" <- as.Date(response$"Set Up Date", origin = "1970-01-01")
      #response$"Due Date" <- as.numeric(as.character(response$"Due Date"))
      response$"Due Date" <- as.Date(response$"Due Date", origin = "1970-01-01")
      response
            
      
    })
    
    output$MasterTracker <- DT::renderDataTable({
      track <- subset(MasterTracker,Manager %in% c(input$Manager))
      track2 <- subset(track,Setup.Month %in% c(input$Month))
      datatable(track2,extensions = 'TableTools', rownames=FALSE, options = list(
        pageLength=100,
        "sDom" = 'T<"clear">lfrtip',
        "oTableTools" = list(
          "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
          "aButtons" = list(
            "copy",
            "print",
            list("sExtends" = "collection",
                 "sButtonText" = "Save",
                 "aButtons" = c("csv","xls"))))))%>%
        formatCurrency(c("CurrBal","Payment.Amount","EffDt"),"$")
      
      
      
      
    })
    
    
    
    output$OfficeTracker <- DT::renderDataTable({
      
      TB <- subset(TR,Manager%in%c(input$MGR))
      TL <- subset(TB,Setup.Month%in%c(input$MTH))
      
      datatable(TL,extensions = 'TableTools', rownames=FALSE,options = list(
        pageLength=3000,
        "sDom" = 'T<"clear">lfrtip',
        "oTableTools" = list(
          "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
          "aButtons" = list(
            "copy",
            "print",
            list("sExtends" = "collection",
                 "sButtonText" = "Save",
                 "aButtons" = c("csv","xls")))))
        
      ) %>%
        formatCurrency(c("Dollar.Budget","Dollar_Initiated","Dollar_Posted"),"$") %>%
        formatPercentage(c("Dollar_BVA","RHB_BVA","RHB_Posted_BVA"))
      
      
      
    })
    
    
    output$MTH <- DT::renderDataTable({
      Dupes <- MasterTracker[duplicated(MasterTracker[,8]),]
      Dupes <- Dupes$"File.."
      Duplicates <- MasterTracker[MasterTracker$"File.." %in% Dupes,]
      
      
      row.names(Duplicates)<-NULL
      Duplicates <- Duplicates[,-c(1,2,3,13:26)]
      datatable(Duplicates,extensions = 'TableTools',rownames=FALSE, options = list(
        pageLength=3000,
        "sDom" = 'T<"clear">lfrtip',
        "oTableTools" = list(
          "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
          "aButtons" = list(
            "copy",
            "print",
            list("sExtends" = "collection",
                 "sButtonText" = "Save",
                 "aButtons" = c("csv","xls"))))))%>%
        formatCurrency(c("CurrBal"),"$")
      
    })
    
    observeEvent(input$refresh, {
      shinyjs::reset("form")
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        write.csv(responses, file)
      }
    )
    outputOptions(output, "table1", suspendWhenHidden = FALSE)
    outputOptions(output, "dupes", suspendWhenHidden = FALSE)
    
  }
                   )
