
box::use(
    shiny[moduleServer, div,NS, h3, p, uiOutput,
           observeEvent,reactiveValues, renderUI,reactiveVal, renderText],
    shiny.fluent[ActionButton.shinyInput,updateActionButton.shinyInput],
    plotly[plotlyOutput, renderPlotly, add_trace, layout, plot_ly, config],
    magrittr[`%>%`]
)

box::use(
    app/view/components/ui/cards,
    app/logic/import_data,
    app/logic/quantitative/affected_person_logic,
    app/logic/functions,
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  cards$card_ui("Betroffene Person",
                ActionButton.shinyInput(ns("toggleButton"), iconProps = list("iconName" = "BarChart4")),
                div(class = "card_content",
                  h3(class = "subtitle","Anzahl der betroffenen Personen :" ,
                  p(class = "description", shiny::textOutput(ns("subtitle")))),
                  # Graph goes here
                  uiOutput(ns("plot_personaf"))
                )
              )

}

#' @export
server <- function(id, filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_personaf <- affected_person_logic$data_personaf(filter)
    data_personaf1 <- affected_person_logic$person_af_sub(filter)
    marker <- list(color = c("#81BEF7"))
    color <- c("#81BEF7", "#81BEF7","#81BEF7", "#81BEF7",
               "#81BEF7",  "#81BEF7", "#81BEF7","#81BEF7","#81BEF7","#81BEF7","#81BEF7",
               "#81BEF7")
    button_state <- reactiveVal(FALSE)

    observeEvent(input$toggleButton, {
      button_state(!button_state())
      if (button_state()) {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "PieSingle"))
      } else {
        updateActionButton.shinyInput(session, "toggleButton", iconProps = list("iconName" = "BarChart4"))
      }
    })

    toggle <- reactiveValues(barplot = TRUE)
    output$plot_personaf <- renderUI({
      if (toggle$barplot) {
        plotlyOutput(ns("barplot"))
      } else {
        plotlyOutput(ns("piechart"))
      }
    })

    output$barplot <- renderPlotly({
      functions$generate_barplot(data_personaf,"Person affected",marker)
    })

    output$piechart <- renderPlotly({
      functions$generate_piechart(data_personaf,"Person affected",color)
    })

    observeEvent(input$toggleButton, {
      toggle$barplot <- !toggle$barplot
    })
    output$subtitle <- shiny::renderText({
      nrow(unique(data_personaf1))
    })


  })
}
