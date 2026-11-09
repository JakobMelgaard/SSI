library(shiny)
library(bslib)

#finder funktionerne
if (file.exists("functions.R")) source("functions.R")

#sorterings dagene
sortering_choices <- function(n) c(as.character(seq_len(n)), paste0(">", n))

ui <- page_sidebar(
    title = "Transit time",
    fillable = TRUE,
    sidebar = sidebar(
        checkboxInput("antal","Antal",value=FALSE),
        numericInput("dage", "Dage", value = 4, min = 1, step = 1),
        checkboxGroupInput("sortering", "Sortering dage",                     
                           choices = sortering_choices(4),
                           selected = "1",
                           inline = TRUE),
        checkboxGroupInput(
            "months", "Vælg måneder",
            choices = måneder,
            selected = tail(måneder, 1)),
        actionButton("all", "Vælg Alle"),
        actionButton("sidste", "Nulstil"),
        checkboxInput("grønland","Grønland og Færøerne?",value=FALSE)
    ),
    plotOutput("plot", height = "100%")
)

server <- function(input, output, session) {
    
    #hvis du trykker vælg alle vælger den alle måneder og hvis du trykker nulstil vælger den den sidste måned
    observeEvent(input$all,  updateCheckboxGroupInput(session, "months", selected = måneder))
    observeEvent(input$sidste, updateCheckboxGroupInput(session, "months", selected = tail(måneder,1)))
    
    #opdaterer checkboxene for sorterings dage hvis antallet af rejsetidslængder ændres
    observeEvent(input$dage, {
        req(input$dage >= 1)
        ch <- sortering_choices(floor(input$dage))
        updateCheckboxGroupInput(session, "sortering",
                                 choices = ch,
                                 selected = intersect(input$sortering, ch))
    })
    
    #kører func når månederne, dagene og sorteringen ændres
    df <- reactive({
        req(input$months, input$dage >= 1, input$sortering)
        func(sort(input$months), dage = input$dage, sortering = input$sortering,grønland = input$grønland,antal = input$antal)
    })
    
    output$plot <- renderPlot({
        plot(df())
    },width="auto")
}

shinyApp(ui, server)