### AUTHOR: AHz
### LAST EDIT: 2021-02-13
### WRITTEN IN: R version 3.5.1
### Purpose: Count unique words in a body of text (shiny app)


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(textreadr)
library(textclean)
library(textshape)
library(wordcloud2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(""),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("data_input_type"),
            conditionalPanel(condition = "input.data_input_type ==`paste`", 
                             uiOutput("paste_text")),
            conditionalPanel(condition = "input.data_input_type ==`upload`", 
                             fileInput("uploadfile", "Choose RTF File",
                                       multiple = FALSE,
                                        accept = ".rtf")),
            uiOutput("break_type"),
            uiOutput("break_contractions")),

        mainPanel(tabsetPanel(id = "main", 
                              tabPanel("data",
                                       DT::dataTableOutput("summary")),
                              tabPanel("text",
                                       textOutput("user_text")),
                              tabPanel("wordcloud",
                                       wordcloud2Output('wordcloud2'))
        )
        ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

 output$data_input_type <- renderUI(
     selectizeInput("data_input_type", "Upload type", choices = as.list(c("Choose one" = "",
                                                                          "Paste my text"= "paste",
                                                                          "Upload an RTF file" = "upload")), 
                    multiple = FALSE)
 )
 

 output$paste_text <- renderUI({
     textInput("paste_text", "Paste text here", placeholder = "...")
 })

 # output$uploadfile<- renderUI({
 #     fileInput("uploadfile", "Choose RTF File",
 #               multiple = FALSE,
 #               accept = c(".rtf"))
 # })

 
     dat <- reactive({
         req(input$data_input_type)
     if(input$data_input_type == "upload"){
         req(input$uploadfile)
         cr <- textreadr::read_rtf(file = input$uploadfile$datapath)
         return(as.vector(cr))
         
     }
    if(input$data_input_type == "paste"){
        req(input$paste_text)
        as.vector(input$paste_text)
    }

     })

 
 output$break_type <- renderUI({
     checkboxInput("break_type", label = "Separate by paragraph")
 })
 output$break_contractions <- renderUI({
     checkboxInput("break_contractions", label = "Break apart contractions")
 })
 
 user_text_clean <- reactive({
     if(input$break_type == TRUE){
         replace_contraction(dat())
     }
     else{
         dat()
     }
 })
 
 dat_formatted <- reactive({
    
     if(input$break_type == TRUE & input$break_contractions == TRUE){
         replace_contraction(dat()) %>%
             split_word() %>% 
             #split_word() %>%
             mtabulate() %>%
             rownames_to_column(var = "paragraph") %>%
             pivot_longer(names_to = "word", values_to = "count", !all_of("paragraph")) %>%
             pivot_wider(names_from = paragraph, values_from = count) 

     }

     if(input$break_type == FALSE & input$break_contractions == TRUE){
         
         split_word(replace_contraction(dat())) %>% 
             mtabulate() %>%
             pivot_longer(names_to = "word", values_to = "count", everything()) %>%
             group_by(word) %>%
             summarize(total_count = sum(count))
         
         
     }
     if(input$break_type == TRUE & input$break_contractions == FALSE){
         split_word(dat()) %>%
            mtabulate() %>%
             rownames_to_column(var = "paragraph") %>%
             pivot_longer(names_to = "word", values_to = "count", !all_of("paragraph")) %>%
             pivot_wider(names_from = paragraph, values_from = count) #%>%
             #full_join(unique_count_total) %>%
             #arrange(desc(count))
         
     }
     else{
         split_word(dat()) %>%
             mtabulate() %>%
             pivot_longer(names_to = "word", values_to = "count", everything()) %>%
             group_by(word) %>%
             summarize(total_count = sum(count)) 
         
     }
 })
 

 output$summary <- DT::renderDataTable({
     DT::datatable(dat_formatted(), options = list(pageLength = 25,
                   autoWidth = TRUE, 
                   order = list(list(2, 'desc'))))
 })

 output$user_text <- renderText({
     user_text_clean()
 })
 
 
 output$wordcloud2 <- renderWordcloud2({
     demoFreq <- dat_formatted() %>% 
         rename(freq = total_count)
     
     wordcloud2(demoFreq)
 })
 
}




# Run the application 
shinyApp(ui = ui, server = server)
